// Eltwise kernel: out[k] = log(x[k]) + y[k] for k in [0, n).
// Pure cpp11 + BLAS (no BLAS used here because log isn't a BLAS primitive;
// this is the "hand-rolled C++" arm of the bake-off).

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <cmath>

[[cpp11::register]]
cpp11::writable::doubles kernel_log_add_cpp(cpp11::doubles x, cpp11::doubles y) {
    const R_xlen_t n = x.size();
    if (y.size() != n) cpp11::stop("x and y must have the same length");
    cpp11::writable::doubles out(n);
    const double *px = REAL(x.data());
    const double *py = REAL(y.data());
    double *pout = REAL(out.data());
    DAFR_PARALLEL_FOR(n >= 10000)
    for (R_xlen_t k = 0; k < n; ++k) {
        pout[k] = std::log(px[k]) + py[k];
    }
    return out;
}
