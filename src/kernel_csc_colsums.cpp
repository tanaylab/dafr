#include <cpp11.hpp>
#include "openmp_shim.h"

[[cpp11::register]]
cpp11::writable::doubles kernel_csc_colsums_cpp(
    cpp11::doubles x,   // nnz values
    cpp11::integers p,  // ncol+1 column pointers (0-based)
    int ncol,
    int threshold
) {
    cpp11::writable::doubles out(ncol);
    const double *px = REAL(x.data());
    const int *pp = INTEGER(p.data());
    double *pout = REAL(out.data());
    DAFR_PARALLEL_FOR(ncol >= threshold)
    for (int j = 0; j < ncol; ++j) {
        double s = 0.0;
        for (int k = pp[j]; k < pp[j + 1]; ++k) {
            s += px[k];
        }
        pout[j] = s;
    }
    return out;
}
