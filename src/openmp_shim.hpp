#ifndef DAFR_OPENMP_SHIM_HPP
#define DAFR_OPENMP_SHIM_HPP

#if defined(DAFR_HAVE_OPENMP) && DAFR_HAVE_OPENMP
  #include <omp.h>
  #define DAFR_PARALLEL_FOR(cond) _Pragma("omp parallel for if(cond) schedule(static)")
  #define DAFR_OMP_THREADS() omp_get_max_threads()
#else
  #define DAFR_PARALLEL_FOR(cond)
  #define DAFR_OMP_THREADS() 1
#endif

#endif
