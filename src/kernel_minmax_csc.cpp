// src/kernel_minmax_csc.cpp
// Per-column or per-row min/max over a dgCMatrix, honouring implicit zeros.
// Template: src/kernel_log_reduce.cpp (same thread-bucket pattern for axis=0).
#include <cpp11.hpp>
#include "openmp_shim.h"
#include <algorithm>
#include <limits>
#include <string>
#include <vector>

[[cpp11::register]]
cpp11::writable::doubles kernel_minmax_csc_cpp(
    cpp11::doubles x,
    cpp11::integers i,
    cpp11::integers p,
    int nrow,
    int ncol,
    int axis,                   // 0 = per-row (collapse cols), 1 = per-col
    std::string variant,        // "Min" | "Max"
    int threshold
) {
    const double *px = REAL(x.data());
    const int *pi = INTEGER(i.data());
    const int *pp = INTEGER(p.data());
    const bool is_min = (variant == "Min");
    const double sentinel = is_min
        ? std::numeric_limits<double>::infinity()
        : -std::numeric_limits<double>::infinity();
    auto fold = [&](double a, double b) { return is_min ? std::min(a, b) : std::max(a, b); };

    if (axis == 1) {
        cpp11::writable::doubles out(ncol);
        double *pout = REAL(out.data());
        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            const int nnz_j = pp[j + 1] - pp[j];
            double v = sentinel;
            for (int k = pp[j]; k < pp[j + 1]; ++k) v = fold(v, px[k]);
            // Fold in implicit zero only when there are implicit zeros
            // (nnz_j < nrow). When nrow == 0 this guard never fires and the
            // sentinel is returned, matching matrixStats::colMaxs/colMins.
            if (nnz_j < nrow) v = fold(v, 0.0);
            pout[j] = v;
        }
        return out;
    }

    // axis == 0: per-row output. Thread-local buckets pattern from
    // kernel_log_reduce_csc_cpp (see src/kernel_log_reduce.cpp:88-125).
    cpp11::writable::doubles out(nrow);
    double *pout = REAL(out.data());
    for (int r = 0; r < nrow; ++r) pout[r] = sentinel;
    std::vector<int> nnz_per_row(nrow, 0);

    const int nthreads = dafr_omp_get_max_threads_capped(ncol, threshold);
    // FIXME: tbuf is O(nthreads × nrow) memory; at scRNA scale (nrow > 1e6)
    // with many threads this may pressure RAM. Consider a sequential fallback
    // for very large nrow.
    std::vector<std::vector<double>> tbuf(nthreads,
        std::vector<double>(nrow, sentinel));
    std::vector<std::vector<int>> tnnz(nthreads, std::vector<int>(nrow, 0));

    DAFR_PARALLEL_FOR(ncol >= threshold)
    for (int j = 0; j < ncol; ++j) {
        const int tid = dafr_omp_get_thread_num();
        auto &tb = tbuf[tid];
        auto &tn = tnnz[tid];
        for (int k = pp[j]; k < pp[j + 1]; ++k) {
            const int r = pi[k];
            tb[r] = fold(tb[r], px[k]);
            tn[r] += 1;
        }
    }
    for (int t = 0; t < nthreads; ++t) {
        for (int r = 0; r < nrow; ++r) {
            pout[r] = fold(pout[r], tbuf[t][r]);
            nnz_per_row[r] += tnnz[t][r];
        }
    }
    DAFR_PARALLEL_FOR(nrow >= threshold)
    for (int r = 0; r < nrow; ++r) {
        if (nnz_per_row[r] < ncol) pout[r] = fold(pout[r], 0.0);
    }
    return out;
}
