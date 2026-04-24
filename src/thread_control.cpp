#include <cpp11.hpp>
#include "openmp_shim.h"

// Thread-count control for dafr's OpenMP-parallel kernels.
//
// CRAN policy caps examples/tests/vignettes at 2 cores. This module
// exposes the minimum primitives needed to respect that cap from R:
//   * `dafr_set_num_threads(n)` — forwards to `omp_set_num_threads`.
//   * `dafr_get_max_threads()`  — reports the current max (post-set).
//
// `omp_set_num_threads` sticks as the process-wide default for
// subsequent `#pragma omp parallel` regions (unless overridden by a
// `num_threads(n)` clause), so setting it once on package load is
// sufficient to cap every kernel. Calls on an OpenMP-less build are
// no-ops.

[[cpp11::register]]
void dafr_set_num_threads(int n) {
#if defined(_OPENMP)
    if (n < 1) n = 1;
    omp_set_num_threads(n);
#else
    (void)n;
#endif
}

[[cpp11::register]]
int dafr_get_max_threads() {
#if defined(_OPENMP)
    return omp_get_max_threads();
#else
    return 1;
#endif
}

[[cpp11::register]]
bool dafr_has_openmp() {
#if defined(_OPENMP)
    return true;
#else
    return false;
#endif
}
