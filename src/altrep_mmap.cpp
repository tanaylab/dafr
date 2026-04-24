#include "altrep_mmap.h"

#include <R_ext/Altrep.h>
#include <R_ext/Rallocators.h>
#include <R_ext/Rdynload.h>

#include <memory>
#include <cstring>

// ---- Thread-safety ----
//
// R's ALTREP API is not thread-safe. The methods in this file read and
// write the ALTREP's data1/data2 slots in place (see writeable Dataptr,
// which materializes by swapping data1 -> R_NilValue and data2 -> a
// freshly allocated vector). Callers MUST ensure that ALTREP methods for
// a given SEXP are never invoked from multiple threads concurrently.
// In particular, if a parallel kernel (OpenMP etc.) needs writable
// access, force materialization in the serial region before entering
// the parallel section.

namespace dafr {

// The ALTREP classes; populated in init_altrep_mmap().
static R_altrep_class_t MmapRealClass;
static R_altrep_class_t MmapIntClass;
static R_altrep_class_t MmapLglClass;

// ---- Common: store shared_ptr<MmapRegion> inside the ALTREP's data1. ----
//
// We wrap the shared_ptr in an EXTPTRSXP and attach a finalizer; the
// EXTPTRSXP becomes data1 on the ALTREP. Dataptr returns the raw region
// pointer. This keeps the MmapRegion alive as long as the R vector is.

static void region_finalizer(SEXP xptr) {
    auto *holder = static_cast<std::shared_ptr<MmapRegion>*>(R_ExternalPtrAddr(xptr));
    if (holder) {
        delete holder;
        R_ClearExternalPtr(xptr);
    }
}

// The xptr carries (region holder, length) so ALTREP objects don't need
// to pre-allocate an N-element dummy vector just to remember their length.
// Length is stored in the xptr's Protected slot as a ScalarReal (R_xlen_t
// fits in double for any realistic R vector).
static SEXP wrap_region(std::shared_ptr<MmapRegion> region, R_xlen_t length) {
    auto *holder = new std::shared_ptr<MmapRegion>(std::move(region));
    SEXP len_sxp = PROTECT(Rf_ScalarReal(static_cast<double>(length)));
    SEXP xptr = PROTECT(R_MakeExternalPtr(holder, R_NilValue, len_sxp));
    // onexit=TRUE is safe: region_finalizer guards on holder non-null, so
    // even if the finalizer is invoked at R exit after a prior GC-triggered
    // run has already deleted the holder, it's a harmless no-op.
    R_RegisterCFinalizerEx(xptr, region_finalizer, TRUE);
    UNPROTECT(2);
    return xptr;
}

static std::shared_ptr<MmapRegion> unwrap_region(SEXP data1) {
    auto *holder = static_cast<std::shared_ptr<MmapRegion>*>(R_ExternalPtrAddr(data1));
    if (!holder) Rf_error("mmap region has been released");
    return *holder;
}

static R_xlen_t xptr_length(SEXP xptr) {
    SEXP len_sxp = R_ExternalPtrProtected(xptr);
    return static_cast<R_xlen_t>(REAL(len_sxp)[0]);
}

// ---- Representation invariants ----
//
// data1 is EXTPTRSXP (region holder + length scalar) pre-materialization,
// or R_NilValue post-materialization.
// data2 is R_NilValue pre-materialization, or the materialized R vector
// post-materialization.
// Length comes from the xptr's protected slot when not yet materialized,
// else from XLENGTH of the materialized vector.

// ---- Real (double) class methods ----

static R_xlen_t mmap_real_length(SEXP x) {
    SEXP d1 = R_altrep_data1(x);
    if (d1 == R_NilValue) return XLENGTH(R_altrep_data2(x));
    return xptr_length(d1);
}

static void *mmap_real_dataptr(SEXP x, Rboolean writeable) {
    // Already materialized? Just return its buffer (idempotent, no re-copy).
    if (R_altrep_data1(x) == R_NilValue) {
        return REAL(R_altrep_data2(x));
    }
    if (writeable) {
        // Materialize: copy mmap bytes into a fresh REALSXP, swap in.
        SEXP region_xptr = R_altrep_data1(x);
        auto region = unwrap_region(region_xptr);
        R_xlen_t n = xptr_length(region_xptr);
        SEXP materialized = PROTECT(Rf_allocVector(REALSXP, n));
        if (n > 0) std::memcpy(REAL(materialized), region->data(), n * sizeof(double));
        R_set_altrep_data1(x, R_NilValue);
        R_set_altrep_data2(x, materialized);
        UNPROTECT(1);
        return REAL(materialized);
    }
    SEXP region_xptr = R_altrep_data1(x);
    auto region = unwrap_region(region_xptr);
    // SAFETY: writeable=FALSE branch. The mmap is PROT_READ, so the backing
    // pages are OS-enforced read-only. The const_cast here exists *only* to
    // satisfy the ALTREP Dataptr signature (void*), not to grant write
    // access. Per the ALTREP convention (R-ints §1.14.4), callers invoking
    // Dataptr with writeable=FALSE promise not to write through the returned
    // pointer — doing so would segfault (SIGBUS on PROT_READ) and is
    // undefined behavior at the C++ level. Callers needing a writeable
    // buffer must pass writeable=TRUE, which materializes a private copy
    // above.
    return const_cast<void*>(region->data());
}

static const void *mmap_real_dataptr_or_null(SEXP x) {
    SEXP region_xptr = R_altrep_data1(x);
    if (region_xptr == R_NilValue) return DATAPTR_RO(R_altrep_data2(x));
    auto region = unwrap_region(region_xptr);
    return region->data();
}

static double mmap_real_elt(SEXP x, R_xlen_t i) {
    const double *p = static_cast<const double*>(mmap_real_dataptr_or_null(x));
    return p[i];
}

static R_xlen_t mmap_real_get_region(SEXP x, R_xlen_t start, R_xlen_t size, double *buf) {
    const double *p = static_cast<const double*>(mmap_real_dataptr_or_null(x));
    R_xlen_t len = mmap_real_length(x);
    if (start < 0 || start >= len) return 0;
    R_xlen_t avail = len - start;
    R_xlen_t n = (size < avail) ? size : avail;
    if (n <= 0) return 0;
    std::memcpy(buf, p + start, n * sizeof(double));
    return n;
}

static Rboolean mmap_real_inspect(SEXP x, int pre, int deep, int pvec,
                                  void (*inspect_subtree)(SEXP, int, int, int)) {
    Rprintf("dafr::MmapRealAltrep length=%lld\n",
            static_cast<long long>(mmap_real_length(x)));
    return TRUE;
}

// Serialized state: materialize then serialize as regular numeric vector.
static SEXP mmap_real_serialized_state(SEXP x) {
    R_xlen_t n = mmap_real_length(x);
    SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
    std::memcpy(REAL(out), mmap_real_dataptr_or_null(x), n * sizeof(double));
    UNPROTECT(1);
    return out;
}
static SEXP mmap_real_unserialize(SEXP, SEXP state) { return state; }

static void init_mmap_real(DllInfo *dll) {
    MmapRealClass = R_make_altreal_class("MmapRealAltrep", "dafr", dll);
    R_set_altrep_Length_method(MmapRealClass, mmap_real_length);
    R_set_altrep_Inspect_method(MmapRealClass, mmap_real_inspect);
    R_set_altrep_Serialized_state_method(MmapRealClass, mmap_real_serialized_state);
    R_set_altrep_Unserialize_method(MmapRealClass, mmap_real_unserialize);
    R_set_altvec_Dataptr_method(MmapRealClass, mmap_real_dataptr);
    R_set_altvec_Dataptr_or_null_method(MmapRealClass, mmap_real_dataptr_or_null);
    R_set_altreal_Elt_method(MmapRealClass, mmap_real_elt);
    R_set_altreal_Get_region_method(MmapRealClass, mmap_real_get_region);
}

SEXP make_mmap_real_altrep(std::shared_ptr<MmapRegion> region, R_xlen_t length) {
    SEXP region_xptr = PROTECT(wrap_region(std::move(region), length));
    SEXP out = R_new_altrep(MmapRealClass, region_xptr, R_NilValue);
    UNPROTECT(1);
    return out;
}

// ---- Int (int32) class — identical structure. ----

static R_xlen_t mmap_int_length(SEXP x) {
    SEXP d1 = R_altrep_data1(x);
    if (d1 == R_NilValue) return XLENGTH(R_altrep_data2(x));
    return xptr_length(d1);
}
static int mmap_int_elt(SEXP x, R_xlen_t i) {
    SEXP rx = R_altrep_data1(x);
    if (rx == R_NilValue) return INTEGER(R_altrep_data2(x))[i];
    auto region = unwrap_region(rx);
    return static_cast<const int*>(region->data())[i];
}
static void *mmap_int_dataptr(SEXP x, Rboolean writeable) {
    if (R_altrep_data1(x) == R_NilValue) {
        return INTEGER(R_altrep_data2(x));
    }
    if (writeable) {
        SEXP rx = R_altrep_data1(x);
        auto region = unwrap_region(rx);
        R_xlen_t n = xptr_length(rx);
        SEXP m = PROTECT(Rf_allocVector(INTSXP, n));
        if (n > 0) std::memcpy(INTEGER(m), region->data(), n * sizeof(int));
        R_set_altrep_data1(x, R_NilValue);
        R_set_altrep_data2(x, m);
        UNPROTECT(1);
        return INTEGER(m);
    }
    SEXP rx = R_altrep_data1(x);
    auto region = unwrap_region(rx);
    // SAFETY: writeable=FALSE branch. The mmap is PROT_READ, so the backing
    // pages are OS-enforced read-only. The const_cast here exists *only* to
    // satisfy the ALTREP Dataptr signature (void*), not to grant write
    // access. Per the ALTREP convention (R-ints §1.14.4), callers invoking
    // Dataptr with writeable=FALSE promise not to write through the returned
    // pointer — doing so would segfault (SIGBUS on PROT_READ) and is
    // undefined behavior at the C++ level. Callers needing a writeable
    // buffer must pass writeable=TRUE, which materializes a private copy
    // above.
    return const_cast<void*>(region->data());
}
static const void *mmap_int_dataptr_or_null(SEXP x) {
    SEXP rx = R_altrep_data1(x);
    if (rx == R_NilValue) return DATAPTR_RO(R_altrep_data2(x));
    auto region = unwrap_region(rx);
    return region->data();
}
static R_xlen_t mmap_int_get_region(SEXP x, R_xlen_t start, R_xlen_t size, int *buf) {
    const int *p = static_cast<const int*>(mmap_int_dataptr_or_null(x));
    R_xlen_t len = mmap_int_length(x);
    if (start < 0 || start >= len) return 0;
    R_xlen_t avail = len - start;
    R_xlen_t n = (size < avail) ? size : avail;
    if (n <= 0) return 0;
    std::memcpy(buf, p + start, n * sizeof(int));
    return n;
}
static SEXP mmap_int_serialized_state(SEXP x) {
    R_xlen_t n = mmap_int_length(x);
    SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
    std::memcpy(INTEGER(out), mmap_int_dataptr_or_null(x), n * sizeof(int));
    UNPROTECT(1);
    return out;
}
static SEXP mmap_int_unserialize(SEXP, SEXP state) { return state; }

static void init_mmap_int(DllInfo *dll) {
    MmapIntClass = R_make_altinteger_class("MmapIntAltrep", "dafr", dll);
    R_set_altrep_Length_method(MmapIntClass, mmap_int_length);
    R_set_altvec_Dataptr_method(MmapIntClass, mmap_int_dataptr);
    R_set_altvec_Dataptr_or_null_method(MmapIntClass, mmap_int_dataptr_or_null);
    R_set_altinteger_Elt_method(MmapIntClass, mmap_int_elt);
    R_set_altinteger_Get_region_method(MmapIntClass, mmap_int_get_region);
    R_set_altrep_Serialized_state_method(MmapIntClass, mmap_int_serialized_state);
    R_set_altrep_Unserialize_method(MmapIntClass, mmap_int_unserialize);
}

SEXP make_mmap_int_altrep(std::shared_ptr<MmapRegion> region, R_xlen_t length) {
    SEXP region_xptr = PROTECT(wrap_region(std::move(region), length));
    SEXP out = R_new_altrep(MmapIntClass, region_xptr, R_NilValue);
    UNPROTECT(1);
    return out;
}

// ---- Lgl (logical, stored as int32). ----

static R_xlen_t mmap_lgl_length(SEXP x) {
    SEXP d1 = R_altrep_data1(x);
    if (d1 == R_NilValue) return XLENGTH(R_altrep_data2(x));
    return xptr_length(d1);
}
static int mmap_lgl_elt(SEXP x, R_xlen_t i) {
    SEXP rx = R_altrep_data1(x);
    if (rx == R_NilValue) return LOGICAL(R_altrep_data2(x))[i];
    auto region = unwrap_region(rx);
    return static_cast<const int*>(region->data())[i];
}
static void *mmap_lgl_dataptr(SEXP x, Rboolean writeable) {
    if (R_altrep_data1(x) == R_NilValue) {
        return LOGICAL(R_altrep_data2(x));
    }
    if (writeable) {
        SEXP rx = R_altrep_data1(x);
        auto region = unwrap_region(rx);
        R_xlen_t n = xptr_length(rx);
        SEXP m = PROTECT(Rf_allocVector(LGLSXP, n));
        if (n > 0) std::memcpy(LOGICAL(m), region->data(), n * sizeof(int));
        R_set_altrep_data1(x, R_NilValue);
        R_set_altrep_data2(x, m);
        UNPROTECT(1);
        return LOGICAL(m);
    }
    SEXP rx = R_altrep_data1(x);
    auto region = unwrap_region(rx);
    // SAFETY: writeable=FALSE branch. The mmap is PROT_READ, so the backing
    // pages are OS-enforced read-only. The const_cast here exists *only* to
    // satisfy the ALTREP Dataptr signature (void*), not to grant write
    // access. Per the ALTREP convention (R-ints §1.14.4), callers invoking
    // Dataptr with writeable=FALSE promise not to write through the returned
    // pointer — doing so would segfault (SIGBUS on PROT_READ) and is
    // undefined behavior at the C++ level. Callers needing a writeable
    // buffer must pass writeable=TRUE, which materializes a private copy
    // above.
    return const_cast<void*>(region->data());
}
static const void *mmap_lgl_dataptr_or_null(SEXP x) {
    SEXP rx = R_altrep_data1(x);
    if (rx == R_NilValue) return DATAPTR_RO(R_altrep_data2(x));
    auto region = unwrap_region(rx);
    return region->data();
}
static R_xlen_t mmap_lgl_get_region(SEXP x, R_xlen_t start, R_xlen_t size, int *buf) {
    const int *p = static_cast<const int*>(mmap_lgl_dataptr_or_null(x));
    R_xlen_t len = mmap_lgl_length(x);
    if (start < 0 || start >= len) return 0;
    R_xlen_t avail = len - start;
    R_xlen_t n = (size < avail) ? size : avail;
    if (n <= 0) return 0;
    std::memcpy(buf, p + start, n * sizeof(int));
    return n;
}
static SEXP mmap_lgl_serialized_state(SEXP x) {
    R_xlen_t n = mmap_lgl_length(x);
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
    std::memcpy(LOGICAL(out), mmap_lgl_dataptr_or_null(x), n * sizeof(int));
    UNPROTECT(1);
    return out;
}
static SEXP mmap_lgl_unserialize(SEXP, SEXP state) { return state; }

static void init_mmap_lgl(DllInfo *dll) {
    MmapLglClass = R_make_altlogical_class("MmapLglAltrep", "dafr", dll);
    R_set_altrep_Length_method(MmapLglClass, mmap_lgl_length);
    R_set_altvec_Dataptr_method(MmapLglClass, mmap_lgl_dataptr);
    R_set_altvec_Dataptr_or_null_method(MmapLglClass, mmap_lgl_dataptr_or_null);
    R_set_altlogical_Elt_method(MmapLglClass, mmap_lgl_elt);
    R_set_altlogical_Get_region_method(MmapLglClass, mmap_lgl_get_region);
    R_set_altrep_Serialized_state_method(MmapLglClass, mmap_lgl_serialized_state);
    R_set_altrep_Unserialize_method(MmapLglClass, mmap_lgl_unserialize);
}

SEXP make_mmap_lgl_altrep(std::shared_ptr<MmapRegion> region, R_xlen_t length) {
    SEXP region_xptr = PROTECT(wrap_region(std::move(region), length));
    SEXP out = R_new_altrep(MmapLglClass, region_xptr, R_NilValue);
    UNPROTECT(1);
    return out;
}

void init_altrep_mmap(DllInfo *dll) {
    init_mmap_real(dll);
    init_mmap_int(dll);
    init_mmap_lgl(dll);
}

} // namespace dafr
