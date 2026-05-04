// Windows-only stubs for MmapZipStore. The POSIX implementation in
// src/mmap_zip_store.cpp is excluded on Windows; cpp11 still generates
// entry points (`_dafr_dafr_mmap_zip_*`) that link against these
// definitions. Each stub raises an R-level error so callers get a clear
// "not supported on Windows" message at runtime instead of a silent
// crash.
//
// The cpp11::register annotations live exclusively in mmap_zip_store.cpp
// — duplicating them here would generate duplicate extern "C" wrappers
// in src/cpp11.cpp because the cpp11 code generator is preprocessor-
// blind. These definitions are wrapped in #ifdef _WIN32 so non-Windows
// builds compile this translation unit to nothing.

#ifdef _WIN32

#include <cpp11.hpp>

#include <cstdint>
#include <string>

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

namespace {
[[noreturn]] inline void mmap_zip_unsupported() {
    cpp11::stop("MmapZipStore is not supported on Windows in this build of dafr");
}
} // namespace

namespace dafr {
class MmapZipStore;  // forward — instances cannot be constructed on Windows
void init_altrep_zip_raw(DllInfo* /*dll*/) {
    // No-op on Windows: the ALTREP RAWSXP class is only registered when
    // MmapZipStore is available, which it isn't on Windows.
}
SEXP make_zip_raw_altrep(MmapZipStore* /*store*/, std::uint64_t /*offset*/,
                         std::uint64_t /*length*/) {
    mmap_zip_unsupported();
}
SEXP make_zip_raw_altrep_with_xptr(MmapZipStore* /*store*/, std::uint64_t /*offset*/,
                                   std::uint64_t /*length*/, SEXP /*store_xptr*/) {
    mmap_zip_unsupported();
}
SEXP make_zip_raw_altrep_writable(MmapZipStore* /*store*/, std::uint64_t /*offset*/,
                                  std::uint64_t /*length*/, SEXP /*store_xptr*/) {
    mmap_zip_unsupported();
}
}  // namespace dafr

SEXP dafr_mmap_zip_open(std::string /*path*/, std::string /*mode*/, double /*max_file_size_double*/) {
    mmap_zip_unsupported();
}
SEXP dafr_mmap_zip_close(SEXP /*xptr*/) { mmap_zip_unsupported(); }
SEXP dafr_mmap_zip_get_bytes(SEXP /*xptr*/, std::string /*key*/) { mmap_zip_unsupported(); }
SEXP dafr_mmap_zip_set_bytes(SEXP /*xptr*/, std::string /*key*/, cpp11::raws /*bytes*/) { mmap_zip_unsupported(); }
SEXP dafr_mmap_zip_delete(SEXP /*xptr*/, std::string /*key*/) { mmap_zip_unsupported(); }
SEXP dafr_mmap_zip_data_offsets(SEXP /*xptr*/) { mmap_zip_unsupported(); }
SEXP dafr_mmap_zip_exists(SEXP /*xptr*/, std::string /*key*/) { mmap_zip_unsupported(); }
SEXP dafr_mmap_zip_list(SEXP /*xptr*/, std::string /*prefix*/) { mmap_zip_unsupported(); }
SEXP dafr_mmap_zip_reserve(SEXP /*xptr*/, std::string /*key*/, double /*size_double*/) { mmap_zip_unsupported(); }
SEXP dafr_mmap_zip_patch_crc(SEXP /*xptr*/, std::string /*key*/) { mmap_zip_unsupported(); }
SEXP dafr_mmap_zip_set_crash_counter(SEXP /*xptr*/, SEXP /*counter*/, SEXP /*ns_env*/) { mmap_zip_unsupported(); }

#endif  // _WIN32
