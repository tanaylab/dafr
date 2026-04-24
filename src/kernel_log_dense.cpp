// Eltwise kernel: y[k] = log(x[k] + eps) / log(base) for k in [0, n).
//
// Dense fast-path for the `Log` eltwise. The existing fast-path in
// .apply_eltwise only fires for the sparsity-preserving case
// (`dgCMatrix` with `eps = 1`, base = e, i.e. log1p on @x). Any other
// parameterisation on dense input (e.g. log2(x + 1e-5), a very common
// MCView transform) falls through to R's `log(x + eps, base)` which
// is single-threaded and allocates a second dense buffer.
//
// This kernel writes the result into a fresh allocation (we cannot
// alias x because the eltwise protocol promises non-destructive
// behaviour) and parallelises over elements.
//
// Works on both vectors and matrices: for matrices the underlying
// storage is a flat column-major double array, so a single
// element-indexed loop covers both shapes.

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <cmath>

namespace {

// Match R's `log(x + eps, base)`:
//   - base == 2    -> log2(x + eps)   (more accurate than log / log(2))
//   - base == 10   -> log10(x + eps)  (same rationale)
//   - base == e    -> log(x + eps)
//   - otherwise    -> log(x + eps) / log(base)
// Returning via an enum-driven switch inside the hot loop would cost a
// branch per element; instead, the caller's dispatch picks one of four
// specialized loops at the top of the function.
enum LogMode { LOG2, LOG10, LOGE, LOG_GENERIC };

inline LogMode log_mode_for(double base) {
    if (base == 2.0)        return LOG2;
    if (base == 10.0)       return LOG10;
    if (base == std::exp(1.0)) return LOGE;
    return LOG_GENERIC;
}

}  // namespace

[[cpp11::register]]
cpp11::writable::doubles kernel_log_dense_vec_cpp(
    cpp11::doubles x,
    double eps,
    double base,
    int threshold
) {
    const R_xlen_t n = x.size();
    cpp11::writable::doubles out(n);
    const double *px = REAL(x.data());
    double *pout = REAL(out.data());
    const LogMode mode = log_mode_for(base);
    const double inv_log_base =
        (mode == LOG_GENERIC) ? (1.0 / std::log(base)) : 0.0;

    switch (mode) {
    case LOG2:
        DAFR_PARALLEL_FOR(n >= threshold)
        for (R_xlen_t k = 0; k < n; ++k) pout[k] = std::log2(px[k] + eps);
        break;
    case LOG10:
        DAFR_PARALLEL_FOR(n >= threshold)
        for (R_xlen_t k = 0; k < n; ++k) pout[k] = std::log10(px[k] + eps);
        break;
    case LOGE:
        DAFR_PARALLEL_FOR(n >= threshold)
        for (R_xlen_t k = 0; k < n; ++k) pout[k] = std::log(px[k] + eps);
        break;
    case LOG_GENERIC:
        DAFR_PARALLEL_FOR(n >= threshold)
        for (R_xlen_t k = 0; k < n; ++k)
            pout[k] = std::log(px[k] + eps) * inv_log_base;
        break;
    }
    return out;
}

[[cpp11::register]]
cpp11::writable::doubles_matrix<> kernel_log_dense_mat_cpp(
    cpp11::doubles_matrix<> m,
    double eps,
    double base,
    int threshold
) {
    const R_xlen_t nrow = m.nrow();
    const R_xlen_t ncol = m.ncol();
    const R_xlen_t n = nrow * ncol;
    cpp11::writable::doubles_matrix<> out(nrow, ncol);
    const double *pm = REAL(m.data());
    double *pout = REAL(out.data());
    const LogMode mode = log_mode_for(base);
    const double inv_log_base =
        (mode == LOG_GENERIC) ? (1.0 / std::log(base)) : 0.0;

    switch (mode) {
    case LOG2:
        DAFR_PARALLEL_FOR(n >= threshold)
        for (R_xlen_t k = 0; k < n; ++k) pout[k] = std::log2(pm[k] + eps);
        break;
    case LOG10:
        DAFR_PARALLEL_FOR(n >= threshold)
        for (R_xlen_t k = 0; k < n; ++k) pout[k] = std::log10(pm[k] + eps);
        break;
    case LOGE:
        DAFR_PARALLEL_FOR(n >= threshold)
        for (R_xlen_t k = 0; k < n; ++k) pout[k] = std::log(pm[k] + eps);
        break;
    case LOG_GENERIC:
        DAFR_PARALLEL_FOR(n >= threshold)
        for (R_xlen_t k = 0; k < n; ++k)
            pout[k] = std::log(pm[k] + eps) * inv_log_base;
        break;
    }
    return out;
}
