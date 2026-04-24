// Per-column bounded top-K partial sort for dense and CSC matrices.
//
// For each column j of an input matrix, return the `k` largest (or
// largest-by-abs) values together with their row indices. Output is
// two k×ncol matrices — `indices` (1-based row indices, integer) and
// `values` (signed values).
//
// Rows with fewer than k qualifying values produce NA-padded tails.
//
// This is the generic primitive the MCView perf pass wants for the
// metacell top1/top2 persistence path: per-column top-K over a sparse
// UMIs matrix avoids materialising the full dense gene×metacell egc
// matrix just to pick two values per column.

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <algorithm>
#include <cmath>
#include <queue>
#include <vector>

using namespace cpp11::literals;

namespace {

struct Entry {
    double score;   // comparison key (abs(x) or x)
    double value;   // signed value
    int row;        // 0-based row index
};

struct ScoreLess {
    bool operator()(const Entry& a, const Entry& b) const {
        return a.score > b.score;  // min-heap: top() is smallest score
    }
};

// Extract the top-k of one column into heap, then drain into sorted output.
inline void finalise_col(
    std::priority_queue<Entry, std::vector<Entry>, ScoreLess>& heap,
    int k,
    int* idx_out,
    double* val_out
) {
    std::vector<Entry> ordered;
    ordered.reserve(heap.size());
    while (!heap.empty()) { ordered.push_back(heap.top()); heap.pop(); }
    std::sort(ordered.begin(), ordered.end(),
              [](const Entry& a, const Entry& b) { return a.score > b.score; });
    const int n_real = static_cast<int>(ordered.size());
    for (int r = 0; r < k; ++r) {
        if (r < n_real) {
            idx_out[r] = ordered[r].row + 1;  // R is 1-based
            val_out[r] = ordered[r].value;
        } else {
            idx_out[r] = NA_INTEGER;
            val_out[r] = NA_REAL;
        }
    }
}

}  // namespace

// Dense input. Matrix is column-major: element (r, c) at m[r + c * nrow].
[[cpp11::register]]
cpp11::writable::list kernel_top_k_per_col_dense_cpp(
    cpp11::doubles_matrix<> m,
    int k,
    bool use_abs,
    int threshold
) {
    if (k <= 0) cpp11::stop("k must be positive");
    const int nrow = m.nrow();
    const int ncol = m.ncol();

    cpp11::writable::integers_matrix<> idx(k, ncol);
    cpp11::writable::doubles_matrix<>   val(k, ncol);
    int*    pidx = INTEGER(idx.data());
    double* pval = REAL(val.data());
    const double* pm = REAL(m.data());

    DAFR_PARALLEL_FOR(ncol >= threshold)
    for (int c = 0; c < ncol; ++c) {
        std::priority_queue<Entry, std::vector<Entry>, ScoreLess> heap;
        const double* col = pm + static_cast<std::size_t>(c) * nrow;
        for (int r = 0; r < nrow; ++r) {
            const double v = col[r];
            if (ISNA(v) || std::isnan(v)) continue;
            const double score = use_abs ? std::fabs(v) : v;
            if (static_cast<int>(heap.size()) < k) {
                heap.push({score, v, r});
            } else if (score > heap.top().score) {
                heap.pop();
                heap.push({score, v, r});
            }
        }
        finalise_col(heap, k, pidx + static_cast<std::size_t>(c) * k,
                     pval + static_cast<std::size_t>(c) * k);
    }

    return cpp11::writable::list({
        "indices"_nm = idx,
        "values"_nm = val
    });
}

// CSC (dgCMatrix) input: x = nnz values, i = 0-based row indices,
// p = column-pointer of length ncol+1.
//
// Only the nonzero entries need to be inspected when use_abs=FALSE:
// zeros can never beat a positive value. For use_abs=TRUE we also
// ignore zeros because abs(0) = 0 < abs(v) for any nonzero v already
// in the heap.
[[cpp11::register]]
cpp11::writable::list kernel_top_k_per_col_csc_cpp(
    cpp11::doubles x,
    cpp11::integers i,
    cpp11::integers p,
    int nrow,
    int ncol,
    int k,
    bool use_abs,
    int threshold
) {
    if (k <= 0) cpp11::stop("k must be positive");
    (void)nrow;  // nrow unused for CSC top-K (row indices come from `i`)

    cpp11::writable::integers_matrix<> idx(k, ncol);
    cpp11::writable::doubles_matrix<>   val(k, ncol);
    int*    pidx = INTEGER(idx.data());
    double* pval = REAL(val.data());
    const double* px = REAL(x.data());
    const int*    pi = INTEGER(i.data());
    const int*    pp = INTEGER(p.data());

    DAFR_PARALLEL_FOR(ncol >= threshold)
    for (int c = 0; c < ncol; ++c) {
        std::priority_queue<Entry, std::vector<Entry>, ScoreLess> heap;
        const int k_begin = pp[c];
        const int k_end   = pp[c + 1];
        for (int kk = k_begin; kk < k_end; ++kk) {
            const double v = px[kk];
            if (ISNA(v) || std::isnan(v)) continue;
            const double score = use_abs ? std::fabs(v) : v;
            if (static_cast<int>(heap.size()) < k) {
                heap.push({score, v, pi[kk]});
            } else if (score > heap.top().score) {
                heap.pop();
                heap.push({score, v, pi[kk]});
            }
        }
        finalise_col(heap, k, pidx + static_cast<std::size_t>(c) * k,
                     pval + static_cast<std::size_t>(c) * k);
    }

    return cpp11::writable::list({
        "indices"_nm = idx,
        "values"_nm = val
    });
}
