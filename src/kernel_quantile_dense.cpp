// src/kernel_quantile_dense.cpp
// Per-column or per-row type-7 quantile on a dense numeric matrix.
// Accepts INTSXP or REALSXP without an up-front storage-mode copy.
//
// Matches stats::quantile(x, q, type = 7):
//   h    = q * (n - 1)
//   lo   = floor(h); hi = ceil(h); frac = h - lo
//   out  = (1 - frac) * v[lo] + frac * v[hi]
//
// NA / NaN semantics (per design spec §6):
//   - Any NA_INTEGER, NaN, or NA_REAL in the column/row -> output is NA_REAL
//     for that column/row.  Short-circuited at value-scan time.
//   - Empty input (n == 0) -> NA_REAL.
//
// axis == 1: per-column (ReduceToRow direction), output length = ncol
// axis == 0: per-row    (ReduceToColumn direction), output length = nrow

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <algorithm>
#include <cmath>
#include <vector>

namespace {

// Compute type-7 quantile on a complete scratch buffer in-place via nth_element.
// Returns NA_REAL if the buffer is empty.  Caller must filter NA/NaN upstream.
double quantile_inplace(std::vector<double>& buf, double q) {
    const int n = static_cast<int>(buf.size());
    if (n == 0) return NA_REAL;
    if (n == 1) return buf[0];
    const double h = q * (n - 1);
    const int lo = static_cast<int>(std::floor(h));
    const int hi = static_cast<int>(std::ceil(h));
    const double frac = h - lo;
    std::nth_element(buf.begin(), buf.begin() + lo, buf.end());
    const double v_lo = buf[lo];
    if (lo == hi) return v_lo;
    // buf is now partitioned around lo; everything >= v_lo is in [lo, end).
    // The hi-th element is the min of that tail -> nth_element(begin+lo+1, begin+hi, end)
    // on the tail.  Simplest: nth_element again from scratch on the full buffer.
    std::nth_element(buf.begin(), buf.begin() + hi, buf.end());
    const double v_hi = buf[hi];
    return (1.0 - frac) * v_lo + frac * v_hi;
}

// Fill scratch buffer from column j (axis=1) of dense matrix.  Returns false
// (and leaves buf unspecified) if any NA/NaN is encountered.
inline bool fill_col_int(const int* xint, int col_offset, int nrow,
                         std::vector<double>& buf) {
    buf.clear();
    buf.reserve(nrow);
    for (int i = 0; i < nrow; ++i) {
        const int vi = xint[col_offset + i];
        if (vi == NA_INTEGER) return false;
        buf.push_back(static_cast<double>(vi));
    }
    return true;
}

inline bool fill_col_dbl(const double* xdbl, int col_offset, int nrow,
                         std::vector<double>& buf) {
    buf.clear();
    buf.reserve(nrow);
    for (int i = 0; i < nrow; ++i) {
        const double v = xdbl[col_offset + i];
        if (ISNAN(v)) return false;     // covers both NA_REAL and NaN
        buf.push_back(v);
    }
    return true;
}

inline bool fill_row_int(const int* xint, int row, int nrow, int ncol,
                         std::vector<double>& buf) {
    buf.clear();
    buf.reserve(ncol);
    for (int j = 0; j < ncol; ++j) {
        const int vi = xint[j * nrow + row];
        if (vi == NA_INTEGER) return false;
        buf.push_back(static_cast<double>(vi));
    }
    return true;
}

inline bool fill_row_dbl(const double* xdbl, int row, int nrow, int ncol,
                         std::vector<double>& buf) {
    buf.clear();
    buf.reserve(ncol);
    for (int j = 0; j < ncol; ++j) {
        const double v = xdbl[j * nrow + row];
        if (ISNAN(v)) return false;
        buf.push_back(v);
    }
    return true;
}

} // namespace

[[cpp11::register]]
cpp11::writable::doubles kernel_quantile_dense_cpp(
    SEXP mat,
    int axis,
    double q,
    int threshold)
{
    const bool is_int = (TYPEOF(mat) == INTSXP);
    SEXP dim = Rf_getAttrib(mat, R_DimSymbol);
    const int nrow = INTEGER(dim)[0];
    const int ncol = INTEGER(dim)[1];

    const int* xint = is_int ? INTEGER_RO(mat) : nullptr;
    const double* xdbl = is_int ? nullptr : REAL_RO(mat);

    if (axis == 1) {
        // Per-column: output length = ncol.
        cpp11::writable::doubles out(ncol);
        double* pout = REAL(out.data());

        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            std::vector<double> buf;
            const int col_offset = j * nrow;
            const bool ok = is_int
                ? fill_col_int(xint, col_offset, nrow, buf)
                : fill_col_dbl(xdbl, col_offset, nrow, buf);
            pout[j] = ok ? quantile_inplace(buf, q) : NA_REAL;
        }
        return out;
    }

    // axis == 0: per-row: output length = nrow.
    cpp11::writable::doubles out(nrow);
    double* pout = REAL(out.data());

    DAFR_PARALLEL_FOR(nrow >= threshold)
    for (int r = 0; r < nrow; ++r) {
        std::vector<double> buf;
        const bool ok = is_int
            ? fill_row_int(xint, r, nrow, ncol, buf)
            : fill_row_dbl(xdbl, r, nrow, ncol, buf);
        pout[r] = ok ? quantile_inplace(buf, q) : NA_REAL;
    }
    return out;
}
