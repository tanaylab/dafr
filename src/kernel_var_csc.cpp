// src/kernel_var_csc.cpp
// One-pass variance / std / VarN / StdN over dgCMatrix columns (axis=1) or
// rows (axis=0). Uses sum_x + sum_x2 + counts pattern; implicit zeros
// contribute 0 to sums but their count is nrow - nnz_in_col.
#include <cpp11.hpp>
#include "openmp_shim.h"
#include <cmath>
#include <string>
#include <vector>

namespace {
inline double derive(const std::string &variant, double var_u,
                     double mean_u, double eps) {
    if (variant == "Var")  return var_u;
    if (variant == "Std")  return std::sqrt(var_u);
    if (variant == "VarN") return var_u / (mean_u + eps);
    if (variant == "StdN") return std::sqrt(var_u) / (mean_u + eps);
    return 0.0;
}
}

[[cpp11::register]]
cpp11::writable::doubles kernel_var_csc_cpp(
    cpp11::doubles x, cpp11::integers i, cpp11::integers p,
    int nrow, int ncol, int axis,
    std::string variant,   // "Var" | "Std" | "VarN" | "StdN"
    double eps,
    int threshold
) {
    const double *px = REAL(x.data());
    const int *pi = INTEGER(i.data());
    const int *pp = INTEGER(p.data());

    if (axis == 1) {
        cpp11::writable::doubles out(ncol);
        double *pout = REAL(out.data());
        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            double sx = 0.0, sxx = 0.0;
            for (int k = pp[j]; k < pp[j + 1]; ++k) {
                const double v = px[k];
                sx += v;
                sxx += v * v;
            }
            const double mean = nrow > 0 ? sx / nrow : 0.0;
            const double var_u = nrow > 0 ? (sxx / nrow - mean * mean) : 0.0;
            pout[j] = derive(variant, var_u < 0 ? 0.0 : var_u, mean, eps);
        }
        return out;
    }

    // axis == 0: per-row. Row-partition: each thread owns a disjoint row
    // range [r0, r1); two threads never write to the same sx_tot/sxx_tot
    // slot. No thread buckets, no serial merge.
    cpp11::writable::doubles out(nrow);
    double *pout = REAL(out.data());
    std::vector<double> sx_tot(nrow, 0.0), sxx_tot(nrow, 0.0);

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
                const double v = px[k];
                sx_tot[r]  += v;
                sxx_tot[r] += v * v;
            }
        }

        // Pass 2: post-process rows in [r0, r1). Each thread owns the
        // reads and writes for its row range.
        for (int r = r0; r < r1; ++r) {
            const double mean = ncol > 0 ? sx_tot[r] / ncol : 0.0;
            const double var_u = ncol > 0 ? (sxx_tot[r] / ncol - mean * mean) : 0.0;
            pout[r] = derive(variant, var_u < 0 ? 0.0 : var_u, mean, eps);
        }
    }
    return out;
}
