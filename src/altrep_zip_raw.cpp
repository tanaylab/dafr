// Slice 17 — ALTREP RAWSXP backed by a slice of MmapZipStore::file_mmap.
// Phase 1: empty stub; the real ALTREP class lands in Phase 3 once the
// read-side parser (Phase 2) returns real entries we can wrap.

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Altrep.h>

#include "mmap_zip_store.h"

namespace dafr {

void init_altrep_zip_raw(DllInfo* /*dll*/) {
    // populated in phase 3
}

SEXP make_zip_raw_altrep(MmapZipStore* /*store*/, uint64_t /*offset*/, uint64_t /*length*/) {
    Rf_error("make_zip_raw_altrep: slice-17 phase 1 stub");
}

}  // namespace dafr
