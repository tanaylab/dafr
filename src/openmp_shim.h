#ifndef DAFR_OPENMP_SHIM_HPP
#define DAFR_OPENMP_SHIM_HPP

/* _OPENMP is defined by the compiler when -fopenmp is active. R's
   SHLIB_OPENMP_CXXFLAGS expands to -fopenmp on platforms with OpenMP
   support and to empty on platforms without it (notably macOS with the
   default system clang), so _OPENMP is the right cross-platform guard. */
#if defined(_OPENMP)
  #include <omp.h>
  /* _Pragma requires a string literal; use the stringify trick so that the
     caller's expression (e.g. n >= 10000) lands in the pragma text. */
  #define DAFR_PRAGMA_STR(x) _Pragma(#x)
  #define DAFR_PARALLEL_FOR(cond) DAFR_PRAGMA_STR(omp parallel for if(cond) schedule(static))
  #define DAFR_OMP_THREADS() omp_get_max_threads()
  inline int dafr_omp_get_thread_num() { return omp_get_thread_num(); }
  inline int dafr_omp_get_max_threads_capped(int work, int threshold) {
      if (work < threshold) return 1;
      return omp_get_max_threads();
  }
#else
  #define DAFR_PARALLEL_FOR(cond)
  #define DAFR_OMP_THREADS() 1
  inline int dafr_omp_get_thread_num() { return 0; }
  inline int dafr_omp_get_max_threads_capped(int /*work*/, int /*threshold*/) { return 1; }
#endif

#endif
