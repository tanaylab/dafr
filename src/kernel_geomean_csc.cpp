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

    // axis == 0: per-row. Row-partition: each thread owns a disjoint row
    // range [r0, r1); writes to pout[r] and nnz_per_row[r] are race-free.
    // No thread buckets, no serial merge.
    cpp11::writable::doubles out(nrow);
    double *pout = REAL(out.data());

    if (ncol == 0) {
        // No columns: result is 0 for every row (degenerate).
        for (int r = 0; r < nrow; ++r) pout[r] = 0.0;
        return out;
    }

    for (int r = 0; r < nrow; ++r) pout[r] = 0.0;
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
                pout[r] += std::log(px[k] + eps);
                nnz_per_row[r] += 1;
            }
        }

        // Pass 2: add zero contribution, derive geometric mean.
        for (int r = r0; r < r1; ++r) {
            double s = pout[r];
            if (has_eps) {
                s += (double)(ncol - nnz_per_row[r]) * log_eps;
                pout[r] = std::exp(s / ncol) - eps;
            } else {
                pout[r] = std::exp(s / ncol);
            }
        }
    }
    return out;
}
