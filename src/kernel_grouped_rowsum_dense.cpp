// src/kernel_grouped_rowsum_dense.cpp
// Grouped rowsum (+ optional sum-of-squares) on a dense numeric matrix with
// double accumulation.  Accepts INTSXP or REALSXP without an up-front copy.
//
// Design goals:
//   - Eliminate the `storage.mode(m) <- "double"` materialization (~2ms on
//     856x683 Int32 mmap UMIs matrix).
//   - Fold the optional sum-of-squares into the same pass so Var/Std/VarN/StdN
//     don't allocate an intermediate m*m matrix.
//   - Single-threaded; no OpenMP.  NA-safe.
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

#include <cpp11.hpp>
#include <cmath>
#include <vector>

using namespace cpp11;

[[cpp11::register]]
list kernel_grouped_rowsum_dense_cpp(
    SEXP mat,
    cpp11::integers groups,
    int ngroups,
    bool need_sq,
    int axis)
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
        // For each column j, accumulate over rows; write result to out(g, j).
        // Column-major memory pattern: sequential read, scattered write to g-row.

        // Allocate output matrices (initialized to 0.0).
        cpp11::writable::doubles_matrix<cpp11::by_column> out_sum(ngroups, ncol);
        cpp11::writable::doubles_matrix<cpp11::by_column> out_sq(need_sq ? ngroups : 0,
                                                                   need_sq ? ncol   : 0);

        // Track which group accumulators have gone NA (bitfield per column).
        // For NA propagation: once NA, stays NA.
        // We use a bool vector per column — allocated once and reused.
        std::vector<bool> na_sum(ngroups, false);
        std::vector<bool> na_sq(ngroups, false);

        for (int j = 0; j < ncol; ++j) {
            const int col_offset = j * nrow;
            // Reset NA tracking for this column.
            std::fill(na_sum.begin(), na_sum.end(), false);
            if (need_sq) std::fill(na_sq.begin(), na_sq.end(), false);

            // Initialize accumulators in output to 0 for this column
            // (the matrix was default-constructed to 0, but we re-zero per column
            //  so we can reuse the same loop structure safely).
            for (int g = 0; g < ngroups; ++g) {
                out_sum(g, j) = 0.0;
                if (need_sq) out_sq(g, j) = 0.0;
            }

            for (int i = 0; i < nrow; ++i) {
                const int g = pg[i] - 1;   // 1-based -> 0-based
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
        // For each column j (group g = groups[j]-1), accumulate over rows;
        // write result to out(i, g).
        // Column-major read, column-major write (g-column in output).

        cpp11::writable::doubles_matrix<cpp11::by_column> out_sum(nrow, ngroups);
        cpp11::writable::doubles_matrix<cpp11::by_column> out_sq(need_sq ? nrow : 0,
                                                                   need_sq ? ngroups : 0);

        // Initialize to 0.0 (default-constructed).
        // NA tracking: per (row, group) — use flat vectors.
        std::vector<bool> na_sum(nrow * ngroups, false);
        std::vector<bool> na_sq(nrow * ngroups, false);

        // Zero-init the output matrices explicitly.
        for (int g = 0; g < ngroups; ++g) {
            for (int i = 0; i < nrow; ++i) {
                out_sum(i, g) = 0.0;
                if (need_sq) out_sq(i, g) = 0.0;
            }
        }

        for (int j = 0; j < ncol; ++j) {
            const int g = pg[j] - 1;   // 1-based -> 0-based
            const int col_offset = j * nrow;

            for (int i = 0; i < nrow; ++i) {
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

                const int idx = g * nrow + i;
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
