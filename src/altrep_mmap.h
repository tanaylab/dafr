#ifndef DAFR_ALTREP_MMAP_HPP
#define DAFR_ALTREP_MMAP_HPP

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Altrep.h>

#include "mmap_region.h"
#include <memory>

namespace dafr {

void init_altrep_mmap(DllInfo *dll);

// Construct an ALTREP numeric (double) SEXP backed by the given mmap region.
// `length` is the number of doubles (nbytes / sizeof(double)).
SEXP make_mmap_real_altrep(std::shared_ptr<MmapRegion> region, R_xlen_t length);

// Int32 variant.
SEXP make_mmap_int_altrep(std::shared_ptr<MmapRegion> region, R_xlen_t length);

// Logical variant (stored as int32 per R semantics).
SEXP make_mmap_lgl_altrep(std::shared_ptr<MmapRegion> region, R_xlen_t length);

} // namespace dafr

#endif
