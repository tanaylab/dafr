// src/kernel_mode_dense.cpp
// Per-column or per-row mode on a dense numeric matrix.
// Accepts INTSXP or REALSXP without up-front storage-mode copy.
//
// Matches .op_mode in R/operations.R bit-exactly:
//   ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]
// i.e., first-encountered wins on count ties.  Scan order = row index for
// axis=1, col index for axis=0.
//
// NA / NaN semantics (design spec §6):
//   - NaN values are routed to a single sentinel bucket (std::hash<double>
//     treats each NaN bit-pattern as a distinct key, and NaN != NaN makes
//     unordered_map unusable for NaN).  If the NaN bucket wins, output is NaN.
//   - NA_REAL is distinguished from other NaN via ISNA and bucketed separately
//     (can win as NA_REAL if most frequent).
//   - Empty input (n == 0) -> NA_REAL.
//
// axis == 1: per-column, output length = ncol
// axis == 0: per-row,    output length = nrow

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <cmath>
#include <limits>
#include <unordered_map>
#include <vector>

namespace {

// Separate sentinel for NaN (non-NA) and NA_REAL, because both are NaN-classed
// but have distinct semantics in R.  We bucket them in dedicated counters
// rather than inside the unordered_map (which can't hash NaN reliably).
struct ModeState {
    std::unordered_map<double, int> counts;
    std::unordered_map<double, int> first_seen;
    int nan_count      = 0;   // plain NaN (not NA)
    int nan_first_seen = std::numeric_limits<int>::max();
    int na_count       = 0;   // NA_REAL
    int na_first_seen  = std::numeric_limits<int>::max();
};

// Observe a value at scan position `pos` (row index for axis=1, col for axis=0).
inline void observe(ModeState& s, double v, int pos) {
    if (ISNA(v)) {
        s.na_count += 1;
        if (pos < s.na_first_seen) s.na_first_seen = pos;
        return;
    }
    if (std::isnan(v)) {
        s.nan_count += 1;
        if (pos < s.nan_first_seen) s.nan_first_seen = pos;
        return;
    }
    auto it = s.counts.find(v);
    if (it == s.counts.end()) {
        s.counts[v] = 1;
        s.first_seen[v] = pos;
    } else {
        it->second += 1;
    }
}

// Pick the mode: highest count wins; on ties, lowest first_seen wins.
// Buckets considered: all finite keys in `counts`, plus the nan and na sentinels.
double pick_mode(const ModeState& s, int n_observed) {
    if (n_observed == 0) return NA_REAL;
    double best_val = NA_REAL;
    int best_count = -1;
    int best_first = std::numeric_limits<int>::max();
    for (const auto& kv : s.counts) {
        const int cnt = kv.second;
        const int fs  = s.first_seen.at(kv.first);
        if (cnt > best_count || (cnt == best_count && fs < best_first)) {
            best_val = kv.first;
            best_count = cnt;
            best_first = fs;
        }
    }
    if (s.nan_count > best_count ||
        (s.nan_count == best_count && s.nan_first_seen < best_first)) {
        best_val = std::nan("");    // return a NaN (non-NA)
        best_count = s.nan_count;
        best_first = s.nan_first_seen;
    }
    if (s.na_count > best_count ||
        (s.na_count == best_count && s.na_first_seen < best_first)) {
        best_val = NA_REAL;
        best_count = s.na_count;
        best_first = s.na_first_seen;
    }
    return best_val;
}

inline double value_int(const int* xint, int idx) {
    const int vi = xint[idx];
    return (vi == NA_INTEGER) ? NA_REAL : static_cast<double>(vi);
}

inline double value_dbl(const double* xdbl, int idx) {
    return xdbl[idx];
}

} // namespace

[[cpp11::register]]
cpp11::writable::doubles kernel_mode_dense_cpp(
    SEXP mat,
    int axis,
    int threshold)
{
    const bool is_int = (TYPEOF(mat) == INTSXP);
    SEXP dim = Rf_getAttrib(mat, R_DimSymbol);
    const int nrow = INTEGER(dim)[0];
    const int ncol = INTEGER(dim)[1];

    const int* xint = is_int ? INTEGER_RO(mat) : nullptr;
    const double* xdbl = is_int ? nullptr : REAL_RO(mat);

    if (axis == 1) {
        cpp11::writable::doubles out(ncol);
        double* pout = REAL(out.data());

        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            ModeState s;
            const int col_offset = j * nrow;
            for (int i = 0; i < nrow; ++i) {
                const double v = is_int
                    ? value_int(xint, col_offset + i)
                    : value_dbl(xdbl, col_offset + i);
                observe(s, v, i);
            }
            pout[j] = pick_mode(s, nrow);
        }
        return out;
    }

    // axis == 0: per-row.
    cpp11::writable::doubles out(nrow);
    double* pout = REAL(out.data());

    DAFR_PARALLEL_FOR(nrow >= threshold)
    for (int r = 0; r < nrow; ++r) {
        ModeState s;
        for (int j = 0; j < ncol; ++j) {
            const double v = is_int
                ? value_int(xint, j * nrow + r)
                : value_dbl(xdbl, j * nrow + r);
            observe(s, v, j);
        }
        pout[r] = pick_mode(s, ncol);
    }
    return out;
}
