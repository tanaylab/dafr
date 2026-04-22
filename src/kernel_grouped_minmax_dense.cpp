// src/kernel_grouped_minmax_dense.cpp
// Grouped Min or Max on a dense INTSXP/REALSXP matrix.  Sibling of
// kernel_grouped_rowsum_dense.cpp (Slice 9b).  Single-threaded, NA-safe.
//
// Design (spec §4.3):
//   - Accumulator init per (i, g): +inf (Min) or -inf (Max).
//   - First non-NA observation replaces sentinel; std::min/max thereafter.
//   - NA propagation: once a cell sees NA, stays NA.
//   - Empty group: sentinel remains; post-process in R dispatch layer
//     (.minmax_empty_to_na) strips sentinels to NA_REAL.
//
// Axis conventions:
//   axis == 2 (G2, row-grouped):  output is ngroups x ncol.
//   axis == 3 (G3, col-grouped):  output is nrow    x ngroups.
//
// Variant: 0 = Min, 1 = Max.

#include <cpp11.hpp>
#include <cmath>
#include <limits>
#include <vector>

using namespace cpp11;

[[cpp11::register]]
cpp11::writable::doubles_matrix<cpp11::by_column>
kernel_grouped_minmax_dense_cpp(
    SEXP mat,
    cpp11::integers groups,
    int ngroups,
    int axis,
    int variant)
{
    const int* pg = INTEGER(groups);
    const bool is_int = (TYPEOF(mat) == INTSXP);
    const bool is_max = (variant == 1);
    const double sentinel = is_max
        ? -std::numeric_limits<double>::infinity()
        :  std::numeric_limits<double>::infinity();

    SEXP dim = Rf_getAttrib(mat, R_DimSymbol);
    const int nrow = INTEGER(dim)[0];
    const int ncol = INTEGER(dim)[1];

    const int* xint = is_int ? INTEGER_RO(mat) : nullptr;
    const double* xdbl = is_int ? nullptr : REAL_RO(mat);

    if (axis == 2) {
        // G2: groups along rows -> output ngroups x ncol.
        cpp11::writable::doubles_matrix<cpp11::by_column> out(ngroups, ncol);
        std::vector<bool> na_flag(ngroups, false);

        for (int j = 0; j < ncol; ++j) {
            // Reset per-column state.
            std::fill(na_flag.begin(), na_flag.end(), false);
            for (int g = 0; g < ngroups; ++g) out(g, j) = sentinel;

            const int col_offset = j * nrow;
            for (int i = 0; i < nrow; ++i) {
                const int g = pg[i] - 1;
                double v;
                bool is_na;
                if (is_int) {
                    const int vi = xint[col_offset + i];
                    is_na = (vi == NA_INTEGER);
                    v = is_na ? 0.0 : static_cast<double>(vi);
                } else {
                    v = xdbl[col_offset + i];
                    is_na = ISNA(v);
                }

                if (na_flag[g]) continue;
                if (is_na) {
                    na_flag[g] = true;
                    out(g, j) = NA_REAL;
                    continue;
                }
                if (is_max) {
                    if (v > out(g, j)) out(g, j) = v;
                } else {
                    if (v < out(g, j)) out(g, j) = v;
                }
            }
        }
        return out;
    }

    // axis == 3: G3 -- groups along cols -> output nrow x ngroups.
    cpp11::writable::doubles_matrix<cpp11::by_column> out(nrow, ngroups);
    std::vector<bool> na_flag(static_cast<size_t>(nrow) * ngroups, false);

    // Initialize all output cells to sentinel.
    for (int g = 0; g < ngroups; ++g) {
        for (int i = 0; i < nrow; ++i) {
            out(i, g) = sentinel;
        }
    }

    for (int j = 0; j < ncol; ++j) {
        const int g = pg[j] - 1;
        const int col_offset = j * nrow;
        for (int i = 0; i < nrow; ++i) {
            double v;
            bool is_na;
            if (is_int) {
                const int vi = xint[col_offset + i];
                is_na = (vi == NA_INTEGER);
                v = is_na ? 0.0 : static_cast<double>(vi);
            } else {
                v = xdbl[col_offset + i];
                is_na = ISNA(v);
            }

            const size_t idx = static_cast<size_t>(g) * nrow + i;
            if (na_flag[idx]) continue;
            if (is_na) {
                na_flag[idx] = true;
                out(i, g) = NA_REAL;
                continue;
            }
            if (is_max) {
                if (v > out(i, g)) out(i, g) = v;
            } else {
                if (v < out(i, g)) out(i, g) = v;
            }
        }
    }
    return out;
}
