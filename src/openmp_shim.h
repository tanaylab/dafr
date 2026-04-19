#ifndef DAFR_OPENMP_SHIM_HPP
#define DAFR_OPENMP_SHIM_HPP

#if defined(DAFR_HAVE_OPENMP) && DAFR_HAVE_OPENMP
  #include <omp.h>
  /* _Pragma requires a string literal; use the stringify trick so that the
     caller's expression (e.g. n >= 10000) lands in the pragma text. */
  #define DAFR_PRAGMA_STR(x) _Pragma(#x)
  #define DAFR_PARALLEL_FOR(cond) DAFR_PRAGMA_STR(omp parallel for if(cond) schedule(static))
  #define DAFR_OMP_THREADS() omp_get_max_threads()
#else
  #define DAFR_PARALLEL_FOR(cond)
  #define DAFR_OMP_THREADS() 1
#endif

#endif
