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

    // axis == 0: per-row output. Row-partition: each thread owns a
    // disjoint row range [r0, r1); writes to pout[r] and nnz_per_row[r]
    // are race-free. No thread buckets, no serial merge.
    cpp11::writable::doubles out(nrow);
    double *pout = REAL(out.data());
    for (int r = 0; r < nrow; ++r) pout[r] = sentinel;
    std::vector<int> nnz_per_row(nrow, 0);

    DAFR_OMP_PARALLEL_IF(nrow >= threshold)
    {
        const int tid = dafr_omp_get_thread_num();
        const int nt  = dafr_omp_get_num_threads();
        const int chunk = (nrow + nt - 1) / nt;
        const int r0 = std::min(nrow, tid * chunk);
        const int r1 = std::min(nrow, r0 + chunk);

        // Pass 1: scan every column; filter by row-range.
        for (int j = 0; j < ncol; ++j) {
            const int k_end = pp[j + 1];
            for (int k = pp[j]; k < k_end; ++k) {
                const int r = pi[k];
                if (r < r0 || r >= r1) continue;
                pout[r] = fold(pout[r], px[k]);
                nnz_per_row[r] += 1;
            }
        }

        // Pass 2: fold in implicit zero for rows with at least one.
        for (int r = r0; r < r1; ++r) {
            if (nnz_per_row[r] < ncol) pout[r] = fold(pout[r], 0.0);
        }
    }
    return out;
}
