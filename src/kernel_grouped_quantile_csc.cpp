// src/kernel_grouped_quantile_csc.cpp
// Grouped quantile (type-7) over dgCMatrix.  Per-(col, group) for G2 and
// per-(row, group) for G3.  Implicit zeros are folded in without being
// materialised (same partitioning trick as kernel_quantile_csc.cpp).
//
// G2 (axis = 2, row-group):  output is ngroups x ncol  (ReduceToColumn)
// G3 (axis = 3, col-group):  output is nrow    x ngroups (ReduceToRow)
//
// Type-7 quantile formula (matches stats::quantile(..., type = 7) and
// .op_median / .op_quantile):
//   h    = q * (n - 1)
//   lo   = floor(h); hi = ceil(h); frac = h - lo
//   result = (1 - frac) * value[lo] + frac * value[hi]
//
// The pick_rank helper partitions the virtual sorted sequence
//   [ negs... | zeros... | positives... ]
// and uses std::nth_element to select the k-th element without materialising
// the zero band.  The helper is destructive so we rebuild neg/pos for the
// hi pick.
//
// Empty group (n_in_group[g] == 0):  output 0 (matches .op_median slow path
// via derive_op convention from Task 8).

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <algorithm>
#include <cmath>
#include <vector>

namespace {

// Destructive pick of k-th element in virtual sorted sequence
// [ neg... | zeros (count = n_zeros) | pos... ].
inline double pick_rank(std::vector<double>& neg, std::vector<double>& pos,
                        int n_zeros, int k) {
    const int n_neg = static_cast<int>(neg.size());
    if (k < n_neg) {
        std::nth_element(neg.begin(), neg.begin() + k, neg.end());
        return neg[k];
    }
    if (k < n_neg + n_zeros) return 0.0;
    const int k_pos = k - n_neg - n_zeros;
    std::nth_element(pos.begin(), pos.begin() + k_pos, pos.end());
    return pos[k_pos];
}

} // anonymous namespace

[[cpp11::register]]
cpp11::writable::doubles_matrix<cpp11::by_column>
kernel_grouped_quantile_csc_cpp(
    cpp11::doubles x, cpp11::integers i, cpp11::integers p,
    int nrow, int ncol,
    cpp11::integers group,       // 1-based; length nrow (G2) or ncol (G3)
    int ngroups,
    cpp11::integers n_in_group,  // length ngroups; caller pre-tabulates
    int axis,                    // 2 = G2 (row-group), 3 = G3 (col-group)
    double q,
    int threshold
) {
    const double *px = REAL(x.data());
    const int    *pi = INTEGER(i.data());
    const int    *pp = INTEGER(p.data());
    const int    *pg = INTEGER(group.data());
    const int    *png = INTEGER(n_in_group.data());

    if (axis == 2) {
        // G2 (row-group): output is ngroups x ncol. Per-column in parallel.
        cpp11::writable::doubles_matrix<cpp11::by_column> out(ngroups, ncol);

        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            // Per-column per-group negative / positive buffers.
            std::vector<std::vector<double>> neg(ngroups), pos(ngroups);
            std::vector<int> nnz_cg(ngroups, 0);
            for (int k = pp[j]; k < pp[j + 1]; ++k) {
                const int g = pg[pi[k]] - 1;
                const double v = px[k];
                if (v < 0.0) neg[g].push_back(v);
                else if (v > 0.0) pos[g].push_back(v);
                nnz_cg[g] += 1;
            }
            for (int g = 0; g < ngroups; ++g) {
                const int n_total = png[g];
                if (n_total <= 0) { out(g, j) = 0.0; continue; }
                const int n_zeros = n_total - nnz_cg[g];
                const double h = q * (n_total - 1);
                const int lo = static_cast<int>(std::floor(h));
                const int hi = static_cast<int>(std::ceil(h));
                const double frac = h - lo;
                if (lo == hi) {
                    out(g, j) = pick_rank(neg[g], pos[g], n_zeros, lo);
                } else {
                    const double v_lo = pick_rank(neg[g], pos[g], n_zeros, lo);
                    // Rebuild neg/pos for hi pick (destructive nth_element).
                    neg[g].clear(); pos[g].clear();
                    for (int k = pp[j]; k < pp[j + 1]; ++k) {
                        if (pg[pi[k]] - 1 != g) continue;
                        const double v = px[k];
                        if (v < 0.0) neg[g].push_back(v);
                        else if (v > 0.0) pos[g].push_back(v);
                    }
                    const double v_hi = pick_rank(neg[g], pos[g], n_zeros, hi);
                    out(g, j) = (1.0 - frac) * v_lo + frac * v_hi;
                }
            }
        }
        return out;
    }

    // axis == 3 (G3, col-group): output is nrow x ngroups.
    // Collect per-(row, group) explicit values in thread-local buffers,
    // merge, then compute quantile per cell.
    //
    // FIXME: thread-bucket storage is O(nthreads * nrow * ngroups) pointers
    // plus O(nnz) values total. For very large nrow * ngroups this can be
    // memory-heavy; consider a row-partitioned fallback if it becomes an
    // issue in benchmarks.
    cpp11::writable::doubles_matrix<cpp11::by_column> out(nrow, ngroups);
    const int nthreads = dafr_omp_get_max_threads_capped(ncol, threshold);
    // tbuf[tid][r + g * nrow] = vector<double> of explicit values in that cell.
    std::vector<std::vector<std::vector<double>>> tbuf(nthreads,
        std::vector<std::vector<double>>((size_t)nrow * (size_t)ngroups));

    DAFR_PARALLEL_FOR(ncol >= threshold)
    for (int j = 0; j < ncol; ++j) {
        const int tid = dafr_omp_get_thread_num();
        const int g = pg[j] - 1;            // constant within column
        auto &buf = tbuf[tid];
        const size_t base = (size_t)g * (size_t)nrow;
        for (int k = pp[j]; k < pp[j + 1]; ++k) {
            buf[base + (size_t)pi[k]].push_back(px[k]);
        }
    }

    // Serial merge of thread buckets into a single accs vector.
    std::vector<std::vector<double>> accs((size_t)nrow * (size_t)ngroups);
    for (int t = 0; t < nthreads; ++t) {
        auto &tb = tbuf[t];
        const size_t N = accs.size();
        for (size_t idx = 0; idx < N; ++idx) {
            if (!tb[idx].empty()) {
                auto &dst = accs[idx];
                dst.insert(dst.end(), tb[idx].begin(), tb[idx].end());
                tb[idx].clear();
                tb[idx].shrink_to_fit();
            }
        }
    }
    // Free thread buckets before the (potentially parallel) compute phase.
    tbuf.clear();

    // Parallel post-process: rows independent, groups independent.
    DAFR_PARALLEL_FOR(nrow >= threshold)
    for (int r = 0; r < nrow; ++r) {
        for (int g = 0; g < ngroups; ++g) {
            const int n_total = png[g];
            if (n_total <= 0) { out(r, g) = 0.0; continue; }
            const size_t idx = (size_t)r + (size_t)g * (size_t)nrow;
            auto &vals = accs[idx];
            std::vector<double> neg, pos;
            neg.reserve(vals.size());
            pos.reserve(vals.size());
            for (double v : vals) {
                if (v < 0.0) neg.push_back(v);
                else if (v > 0.0) pos.push_back(v);
            }
            const int n_zeros = n_total -
                static_cast<int>(neg.size()) - static_cast<int>(pos.size());
            const double h = q * (n_total - 1);
            const int lo = static_cast<int>(std::floor(h));
            const int hi = static_cast<int>(std::ceil(h));
            const double frac = h - lo;
            if (lo == hi) {
                out(r, g) = pick_rank(neg, pos, n_zeros, lo);
            } else {
                const double v_lo = pick_rank(neg, pos, n_zeros, lo);
                // Rebuild for hi pick.
                neg.clear(); pos.clear();
                for (double v : vals) {
                    if (v < 0.0) neg.push_back(v);
                    else if (v > 0.0) pos.push_back(v);
                }
                const double v_hi = pick_rank(neg, pos, n_zeros, hi);
                out(r, g) = (1.0 - frac) * v_lo + frac * v_hi;
            }
        }
    }
    return out;
}
