#ifndef DAFR_KERNEL_GROUPED_ACC_H
#define DAFR_KERNEL_GROUPED_ACC_H

// Shared accumulator + op-derivation for kernel_grouped_reduce_csc.cpp
// and kernel_grouped_reduce_dense.cpp.
//
// Kept header-only (inline) because both translation units are tiny and
// there is no separate object for shared symbols; cpp11 glue is per-file.

#include <algorithm>
#include <cmath>
#include <limits>
#include <string>

namespace dafr_grouped {

// Per-group / per-(row,group) accumulator. push() is only called for
// explicit entries; implicit zeros are accounted for by
// `n_zeros = n_total - a.nnz` in derive_op().
struct Acc {
    double sum_x = 0.0;
    double sum_x2 = 0.0;
    double log_sum = 0.0;   // Σ log(x + eps) over explicit entries; only
                            // computed when need_log == true (GeoMean only)
    int nnz = 0;
    double min_x =  std::numeric_limits<double>::infinity();
    double max_x = -std::numeric_limits<double>::infinity();
    bool need_log = false;  // set at construction; skip log() for non-GeoMean
    inline void push(double v, double eps) {
        sum_x   += v;
        sum_x2  += v * v;
        if (need_log) log_sum += std::log(v + eps);
        nnz     += 1;
        if (v < min_x) min_x = v;
        if (v > max_x) max_x = v;
    }
};

inline void acc_merge(Acc &dst, const Acc &src) {
    dst.sum_x   += src.sum_x;
    dst.sum_x2  += src.sum_x2;
    dst.log_sum += src.log_sum;
    dst.nnz     += src.nnz;
    if (src.min_x < dst.min_x) dst.min_x = src.min_x;
    if (src.max_x > dst.max_x) dst.max_x = src.max_x;
}

// Derive the per-op scalar output from an Acc covering n_total elements.
// Conventions (all cross-checked against tests and .op_* in R/operations.R):
//   - Empty group (n_total == 0): returns 0 for every op (matches the
//     slow-path contract used by tests: `if (n == 0L) return 0`).
//   - Min/Max fold in an implicit zero whenever n_zeros > 0. When the
//     group has no explicit entries at all, the answer is 0 (the sole
//     value is the implicit zero).
//   - Var uses the UNCORRECTED formula sum_x2/n - mean^2 (not n/(n-1)).
//   - VarN/StdN use a LINEAR denominator (mean + eps), not squared.
//   - GeoMean matches .op_geomean:
//       eps == 0 -> exp(mean(log(x))); if any implicit zero exists,
//                   log(0) = -Inf => result is 0.
//       eps >  0 -> exp((Σlog(x+eps) + n_zeros·log(eps))/n_total) - eps.
//   - For dense callers a.nnz == n_total so n_zeros = 0; the same
//     derive_op then collapses to the ungrouped closed forms.
inline double derive_op(const std::string &op, const Acc &a,
                        int n_total, double eps) {
    if (n_total <= 0) return 0.0;
    const int n_zeros = n_total - a.nnz;
    const double sum_total = a.sum_x;            // implicit zeros add 0
    const double mean = sum_total / (double)n_total;
    const double var_u_raw = a.sum_x2 / (double)n_total - mean * mean;
    const double var_u = var_u_raw < 0.0 ? 0.0 : var_u_raw;
    if (op == "Sum")    return sum_total;
    if (op == "Mean")   return mean;
    if (op == "Min") {
        if (a.nnz == 0) return 0.0;              // all implicit zeros
        return n_zeros > 0 ? std::min(a.min_x, 0.0) : a.min_x;
    }
    if (op == "Max") {
        if (a.nnz == 0) return 0.0;
        return n_zeros > 0 ? std::max(a.max_x, 0.0) : a.max_x;
    }
    if (op == "Var")    return var_u;
    if (op == "Std")    return std::sqrt(var_u);
    if (op == "VarN")   return var_u / (mean + eps);             // LINEAR
    if (op == "StdN")   return std::sqrt(var_u) / (mean + eps);
    if (op == "GeoMean") {
        if (eps == 0.0) {
            if (n_zeros > 0) return 0.0;          // log(0) = -Inf
            return std::exp(a.log_sum / (double)n_total);
        }
        const double s = a.log_sum + (double)n_zeros * std::log(eps);
        return std::exp(s / (double)n_total) - eps;
    }
    return 0.0;   // unknown op
}

}  // namespace dafr_grouped

#endif  // DAFR_KERNEL_GROUPED_ACC_H
