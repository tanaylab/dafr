// src/kernel_grouped_rowsum_dense.cpp
// Grouped rowsum (+ optional sum-of-squares) on a dense numeric matrix with
// double accumulation.  Accepts INTSXP or REALSXP without an up-front copy.
//
// Design goals:
//   - Eliminate the `storage.mode(m) <- "double"` materialization (~2ms on
//     856x683 Int32 mmap UMIs matrix).
//   - Fold the optional sum-of-squares into the same pass so Var/Std/VarN/StdN
//     don't allocate an intermediate m*m matrix.
//   - NA-safe.
//
// Returns list(sum = doubles_matrix, sq = doubles_matrix | NULL).
//   sum: accumulated column sums per group (axis==2) or row sums per group (axis==3).
//   sq:  accumulated sum-of-squares per group; NULL when need_sq == FALSE.
//
// Axis conventions (same as kernel_grouped_reduce_dense_cpp):
//   axis == 2 (G2, row-grouped):  output is ngroups x ncol.
//   axis == 3 (G3, col-grouped):  output is nrow x ngroups.
//
// NA semantics: NA_integer_ / NA_real_ in any input cell propagates to the
// corresponding group accumulator and remains NA in the output.
//
// Parallelism:
//   axis == 2: column-parallel — each column's state is independent.
//   axis == 3: row-partition — output nrow x ngroups has writes keyed by
//     row index i, so each thread owns a disjoint [i0, i1) range.

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <algorithm>
#include <cmath>
#include <vector>

using namespace cpp11;

[[cpp11::register]]
list kernel_grouped_rowsum_dense_cpp(
    SEXP mat,
    cpp11::integers groups,
    int ngroups,
    bool need_sq,
    int axis,
    int threshold)
{
    const int *pg = INTEGER(groups);
    const bool is_int = (TYPEOF(mat) == INTSXP);

    // Dimensions: R stores matrices column-major.
    // NROW / NCOL via R's dim attribute (works for both INTSXP and REALSXP).
    SEXP dim = Rf_getAttrib(mat, R_DimSymbol);
    const int nrow = INTEGER(dim)[0];
    const int ncol = INTEGER(dim)[1];

    // Raw read-only pointers — work with ALTREP without forcing materialization.
    const int    *xint  = is_int ? INTEGER_RO(mat) : nullptr;
    const double *xdbl  = is_int ? nullptr         : REAL_RO(mat);

    if (axis == 2) {
        // G2: groups along rows.  output: ngroups x ncol.
        // Column-parallel: each column's (na_sum, na_sq, out columns) state
        // is independent, so splitting ncol across threads is race-free.

        cpp11::writable::doubles_matrix<cpp11::by_column> out_sum(ngroups, ncol);
        cpp11::writable::doubles_matrix<cpp11::by_column> out_sq(need_sq ? ngroups : 0,
                                                                   need_sq ? ncol   : 0);

        DAFR_OMP_PARALLEL_IF(ncol >= threshold)
        {
            const int tid = dafr_omp_get_thread_num();
            const int nt  = dafr_omp_get_num_threads();
            const int chunk = (ncol + nt - 1) / nt;
            const int j0 = std::min(ncol, tid * chunk);
            const int j1 = std::min(ncol, j0 + chunk);
            std::vector<bool> na_sum(ngroups, false);
            std::vector<bool> na_sq(ngroups, false);

            for (int j = j0; j < j1; ++j) {
                const int col_offset = j * nrow;
                std::fill(na_sum.begin(), na_sum.end(), false);
                if (need_sq) std::fill(na_sq.begin(), na_sq.end(), false);

                for (int g = 0; g < ngroups; ++g) {
                    out_sum(g, j) = 0.0;
                    if (need_sq) out_sq(g, j) = 0.0;
                }

                for (int i = 0; i < nrow; ++i) {
                    const int g = pg[i] - 1;
                    double v;
                    bool is_na;
                    if (is_int) {
                        const int vi = xint[col_offset + i];
                        is_na = (vi == NA_INTEGER);
                        v = is_na ? 0.0 : (double)vi;
                    } else {
                        v = xdbl[col_offset + i];
                        is_na = ISNA(v);
                        if (is_na) v = 0.0;
                    }

                    if (!na_sum[g]) {
                        if (is_na) {
                            na_sum[g] = true;
                            out_sum(g, j) = NA_REAL;
                        } else {
                            out_sum(g, j) += v;
                        }
                    }
                    if (need_sq && !na_sq[g]) {
                        if (is_na) {
                            na_sq[g] = true;
                            out_sq(g, j) = NA_REAL;
                        } else {
                            out_sq(g, j) += v * v;
                        }
                    }
                }
            }
        }

        using namespace cpp11::literals;
        if (need_sq) {
            return cpp11::writable::list({
                "sum"_nm = out_sum,
                "sq"_nm  = out_sq
            });
        } else {
            return cpp11::writable::list({
                "sum"_nm = out_sum,
                "sq"_nm  = R_NilValue
            });
        }

    } else {
        // axis == 3: G3, groups along cols.  output: nrow x ngroups.
        // Row-partition: each thread owns a disjoint row range [i0, i1).
        // All writes to out_sum(i, g), out_sq(i, g), na_sum[g*nrow+i],
        // na_sq[g*nrow+i] for i in that range are race-free.

        cpp11::writable::doubles_matrix<cpp11::by_column> out_sum(nrow, ngroups);
        cpp11::writable::doubles_matrix<cpp11::by_column> out_sq(need_sq ? nrow : 0,
                                                                   need_sq ? ngroups : 0);

        // NA tracking: per (row, group) — flat vectors allocated once,
        // written into disjoint slots by each row-owning thread.
        std::vector<bool> na_sum(static_cast<size_t>(nrow) * ngroups, false);
        std::vector<bool> na_sq(need_sq ? static_cast<size_t>(nrow) * ngroups : 0,
                                false);

        DAFR_OMP_PARALLEL_IF(nrow >= threshold)
        {
            const int tid = dafr_omp_get_thread_num();
            const int nt  = dafr_omp_get_num_threads();
            const int chunk = (nrow + nt - 1) / nt;
            const int i0 = std::min(nrow, tid * chunk);
            const int i1 = std::min(nrow, i0 + chunk);

            // Initialize [i0, i1) rows of out_sum / out_sq to 0.
            for (int g = 0; g < ngroups; ++g) {
                for (int i = i0; i < i1; ++i) {
                    out_sum(i, g) = 0.0;
                    if (need_sq) out_sq(i, g) = 0.0;
                }
            }

            // Scan every column; filter by row-range.
            for (int j = 0; j < ncol; ++j) {
                const int g = pg[j] - 1;
                const int col_offset = j * nrow;

                for (int i = i0; i < i1; ++i) {
                    double v;
                    bool is_na;
                    if (is_int) {
                        const int vi = xint[col_offset + i];
                        is_na = (vi == NA_INTEGER);
                        v = is_na ? 0.0 : (double)vi;
                    } else {
                        v = xdbl[col_offset + i];
                        is_na = ISNA(v);
                        if (is_na) v = 0.0;
                    }

                    const size_t idx = static_cast<size_t>(g) * nrow + i;
                    if (!na_sum[idx]) {
                        if (is_na) {
                            na_sum[idx] = true;
                            out_sum(i, g) = NA_REAL;
                        } else {
                            out_sum(i, g) += v;
                        }
                    }
                    if (need_sq && !na_sq[idx]) {
                        if (is_na) {
                            na_sq[idx] = true;
                            out_sq(i, g) = NA_REAL;
                        } else {
                            out_sq(i, g) += v * v;
                        }
                    }
                }
            }
        }

        using namespace cpp11::literals;
        if (need_sq) {
            return cpp11::writable::list({
                "sum"_nm = out_sum,
                "sq"_nm  = out_sq
            });
        } else {
            return cpp11::writable::list({
                "sum"_nm = out_sum,
                "sq"_nm  = R_NilValue
            });
        }
    }
}
