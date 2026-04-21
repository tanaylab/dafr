// Fused log+reduce kernel.
//
// Single-pass log(x + eps)/log(base) accumulated into a row or column
// sum/mean. Two variants: dense matrix (column-major like R's storage),
// CSC (Matrix::dgCMatrix layout: x = nnz values, i = row indices, p =
// column-pointer of length ncol+1). For CSC + Sum/Mean this avoids the
// dense intermediate the unfused path allocates.

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <cmath>
#include <string>
#include <vector>

[[cpp11::register]]
cpp11::writable::doubles kernel_log_reduce_dense_cpp(
    cpp11::doubles_matrix<> m,
    double eps,
    double base,
    int axis,           // 0 = row reduction, 1 = column reduction
    std::string reducer,
    int threshold
) {
    const int nrow = m.nrow();
    const int ncol = m.ncol();
    const double *pm = REAL(m.data());
    const double inv_log_base = 1.0 / std::log(base);
    const bool is_mean = (reducer == "Mean");

    if (axis == 0) {
        cpp11::writable::doubles out(nrow);
        double *pout = REAL(out.data());
        DAFR_PARALLEL_FOR(nrow >= threshold)
        for (int r = 0; r < nrow; ++r) {
            double s = 0.0;
            for (int c = 0; c < ncol; ++c) {
                s += std::log(pm[r + c * nrow] + eps) * inv_log_base;
            }
            pout[r] = is_mean ? s / ncol : s;
        }
        return out;
    } else {
        cpp11::writable::doubles out(ncol);
        double *pout = REAL(out.data());
        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int c = 0; c < ncol; ++c) {
            double s = 0.0;
            for (int r = 0; r < nrow; ++r) {
                s += std::log(pm[r + c * nrow] + eps) * inv_log_base;
            }
            pout[c] = is_mean ? s / nrow : s;
        }
        return out;
    }
}

[[cpp11::register]]
cpp11::writable::doubles kernel_log_reduce_csc_cpp(
    cpp11::doubles x,         // nnz values
    cpp11::integers i,        // row indices, 0-based
    cpp11::integers p,        // ncol+1 column-pointers, 0-based
    int nrow,
    int ncol,
    double eps,
    double base,
    int axis,
    std::string reducer,
    int threshold
) {
    const double *px = REAL(x.data());
    const int *pi = INTEGER(i.data());
    const int *pp = INTEGER(p.data());
    const double inv_log_base = 1.0 / std::log(base);
    const double zero_log = std::log(eps) * inv_log_base;
    const bool is_mean = (reducer == "Mean");

    if (axis == 0) {
        cpp11::writable::doubles out(nrow);
        double *pout = REAL(out.data());
        // Each row receives:
        //   sum_explicit = sum over nonzero entries in this row of log(x+eps)/log(base)
        //   sum_zeros    = (ncol - nnz_in_this_row) * log(eps)/log(base)
        // Total for Sum = sum_explicit + sum_zeros; Mean divides by ncol.
        //
        // Per-thread accumulator buffers avoid write contention on per-row
        // counters. Post-parallel reduction is O(nthreads * nrow) — cheap
        // vs O(nnz) for typical UMI matrices.
        for (int r = 0; r < nrow; ++r) { pout[r] = 0.0; }
        std::vector<int> nnz_per_row(nrow, 0);

        const int nthreads = dafr_omp_get_max_threads_capped(ncol, threshold);
        std::vector<std::vector<double>> tsum(nthreads,
            std::vector<double>(nrow, 0.0));
        std::vector<std::vector<int>>    tnnz(nthreads,
            std::vector<int>(nrow, 0));

        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            const int tid = dafr_omp_get_thread_num();
            std::vector<double> &ts = tsum[tid];
            std::vector<int>    &tn = tnnz[tid];
            for (int k = pp[j]; k < pp[j + 1]; ++k) {
                const int r = pi[k];
                ts[r] += std::log(px[k] + eps) * inv_log_base;
                tn[r] += 1;
            }
        }

        // Reduce thread buffers serially into outputs.
        for (int t = 0; t < nthreads; ++t) {
            const std::vector<double> &ts = tsum[t];
            const std::vector<int>    &tn = tnnz[t];
            for (int r = 0; r < nrow; ++r) {
                pout[r]        += ts[r];
                nnz_per_row[r] += tn[r];
            }
        }

        DAFR_PARALLEL_FOR(nrow >= threshold)
        for (int r = 0; r < nrow; ++r) {
            const int zeros = ncol - nnz_per_row[r];
            pout[r] += zeros * zero_log;
            if (is_mean) pout[r] /= ncol;
        }
        return out;
    } else {
        cpp11::writable::doubles out(ncol);
        double *pout = REAL(out.data());
        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            const int nnz_j = pp[j + 1] - pp[j];
            double s = 0.0;
            for (int k = pp[j]; k < pp[j + 1]; ++k) {
                s += std::log(px[k] + eps) * inv_log_base;
            }
            const int zeros = nrow - nnz_j;
            s += zeros * zero_log;
            pout[j] = is_mean ? s / nrow : s;
        }
        return out;
    }
}
