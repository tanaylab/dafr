// Geometric mean with eps regularisation over dgCMatrix.
//
// Mirrors the R function .op_geomean in R/operations.R:
//   eps == 0: exp(mean(log(x)))
//   eps >  0: exp(mean(log(x + eps))) - eps
//
// For a sparse column j with nnz_j explicit values and (nrow - nnz_j)
// implicit zeros:
//   mean(log(x + eps))
//     = (sum_{k in nnz} log(x_k + eps) + (nrow - nnz_j) * log(0 + eps)) / nrow
//
// Back-transform matches the R formula exactly:
//   eps == 0: exp(s / n)
//   eps >  0: exp(s / n) - eps
//
// axis == 1: per-column reduction (ReduceToRow)
// axis == 0: per-row    reduction (ReduceToColumn)

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <cmath>
#include <vector>

[[cpp11::register]]
cpp11::writable::doubles kernel_geomean_csc_cpp(
    cpp11::doubles x,
    cpp11::integers i,
    cpp11::integers p,
    int nrow,
    int ncol,
    int axis,
    double eps,
    int threshold
) {
    const double *px = REAL(x.data());
    const int *pi = INTEGER(i.data());
    const int *pp = INTEGER(p.data());
    const double log_eps = (eps > 0.0) ? std::log(eps) : 0.0;
    const bool has_eps = (eps > 0.0);

    if (axis == 1) {
        // Per-column: one output per column. CSC layout makes this easy.
        cpp11::writable::doubles out(ncol);
        double *pout = REAL(out.data());

        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            if (nrow == 0) { pout[j] = 0.0; continue; }
            const int start = pp[j];
            const int end   = pp[j + 1];
            const int nnz_j = end - start;
            double s = 0.0;
            for (int k = start; k < end; ++k) {
                s += std::log(px[k] + eps);
            }
            if (has_eps) {
                // implicit zeros contribute log(0 + eps) = log(eps) each
                s += (double)(nrow - nnz_j) * log_eps;
                pout[j] = std::exp(s / nrow) - eps;
            } else {
                // eps == 0: no correction for structural zeros
                // (caller should only pass fully-nonzero columns in this path)
                pout[j] = std::exp(s / nrow);
            }
        }
        return out;
    }

    // axis == 0: per-row. Thread-bucket accumulation pattern (same as
    // kernel_log_reduce_csc_cpp, kernel_var_csc_cpp).
    cpp11::writable::doubles out(nrow);
    double *pout = REAL(out.data());

    if (ncol == 0) {
        // No columns: result is 0 for every row (degenerate).
        for (int r = 0; r < nrow; ++r) pout[r] = 0.0;
        return out;
    }

    // FIXME: tsum/tnnz are O(nthreads * nrow). For scRNA-seq-scale inputs
    // (nrow > 1e6), consider a sequential fallback.
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
            ts[r] += std::log(px[k] + eps);
            tn[r] += 1;
        }
    }

    // Initialise output to zero before serial reduction.
    for (int r = 0; r < nrow; ++r) pout[r] = 0.0;
    std::vector<int> nnz_per_row(nrow, 0);

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
        double s = pout[r];
        if (has_eps) {
            s += (double)(ncol - nnz_per_row[r]) * log_eps;
            pout[r] = std::exp(s / ncol) - eps;
        } else {
            pout[r] = std::exp(s / ncol);
        }
    }
    return out;
}
