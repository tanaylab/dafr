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

    // axis == 0: per-row. Thread buckets for sum_x, sum_x2.
    // FIXME: tbuf is O(nthreads * nrow). For scRNA-seq-scale inputs
    // (nrow > 1e6), consider a sequential fallback.
    cpp11::writable::doubles out(nrow);
    double *pout = REAL(out.data());
    std::vector<double> sx_tot(nrow, 0.0), sxx_tot(nrow, 0.0);

    const int nthreads = dafr_omp_get_max_threads_capped(ncol, threshold);
    std::vector<std::vector<double>> tsx(nthreads, std::vector<double>(nrow, 0.0));
    std::vector<std::vector<double>> tsxx(nthreads, std::vector<double>(nrow, 0.0));

    DAFR_PARALLEL_FOR(ncol >= threshold)
    for (int j = 0; j < ncol; ++j) {
        const int tid = dafr_omp_get_thread_num();
        auto &tsa = tsx[tid];
        auto &tsb = tsxx[tid];
        for (int k = pp[j]; k < pp[j + 1]; ++k) {
            const int r = pi[k];
            const double v = px[k];
            tsa[r] += v;
            tsb[r] += v * v;
        }
    }
    for (int t = 0; t < nthreads; ++t) {
        for (int r = 0; r < nrow; ++r) {
            sx_tot[r]  += tsx[t][r];
            sxx_tot[r] += tsxx[t][r];
        }
    }
    DAFR_PARALLEL_FOR(nrow >= threshold)
    for (int r = 0; r < nrow; ++r) {
        const double mean = ncol > 0 ? sx_tot[r] / ncol : 0.0;
        const double var_u = ncol > 0 ? (sxx_tot[r] / ncol - mean * mean) : 0.0;
        pout[r] = derive(variant, var_u < 0 ? 0.0 : var_u, mean, eps);
    }
    return out;
}
