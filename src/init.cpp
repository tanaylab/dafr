#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

// Kernel and ALTREP entry points are registered here as they are added.
// cpp11-generated bindings in src/cpp11.cpp register themselves via their
// own R_init_dafr_cpp11 hook, so this file only needs to register non-cpp11
// native routines and call init_altrep_mmap() in Phase D.

extern "C" {

static const R_CallMethodDef CallEntries[] = {
    {nullptr, nullptr, 0}
};

attribute_visible void R_init_dafr(DllInfo *dll) {
    R_registerRoutines(dll, nullptr, CallEntries, nullptr, nullptr);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}

} // extern "C"
