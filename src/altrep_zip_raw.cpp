// Slice 17 — ALTREP RAWSXP backed by a slice of MmapZipStore::file_mmap.
// Phase 3: real ALTREP class with close-time deactivation safety net.
//
// Design:
//   - data1: EXTPTRSXP carrying ZipRawState* { shared_ptr<AltrepRawSlot> slot;
//     const uint8_t* data; }. The xptr's protected slot holds the store xptr
//     (so R's GC keeps the impl alive while any ALTREP references it).
//   - data2: R_NilValue pre-materialization, or the RAWSXP after a writeable
//     Dataptr() call materializes a private copy.
//   - The slot's `deactivated` flag is flipped to true when the store closes
//     (or is GC'd). After that, methods short-circuit to a stable empty / inert
//     buffer so derefs don't segfault.

#include <cpp11.hpp>

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Altrep.h>

#include <cstring>
#include <memory>

#include "mmap_zip_store.h"

namespace dafr {

// Stable address returned for deactivated vectors. Callers must treat it as
// read-only; deactivated vectors should never be written through. A single
// byte is enough since `Length` returns 0 for deactivated vectors and
// downstream code shouldn't deref past the end.
static const Rbyte inert_buffer[1] = { 0x00 };

// Heap struct attached to the EXTPTRSXP inside data1.
struct ZipRawState {
    std::shared_ptr<AltrepRawSlot> slot;
    const uint8_t* data;  // pointer into the impl's file_mmap (cached at make-time)
};

static R_altrep_class_t ZipRawClass;

static void zip_raw_state_finalizer(SEXP xptr) {
    auto* st = static_cast<ZipRawState*>(R_ExternalPtrAddr(xptr));
    if (st) {
        delete st;
        R_ClearExternalPtr(xptr);
    }
}

static SEXP wrap_state(std::shared_ptr<AltrepRawSlot> slot, const uint8_t* data,
                       SEXP store_xptr) {
    auto* st = new ZipRawState{std::move(slot), data};
    // Tag is R_NilValue; protected slot is the store xptr to keep the impl
    // alive across GC for as long as the ALTREP holds the state.
    SEXP xptr = PROTECT(R_MakeExternalPtr(st, R_NilValue, store_xptr));
    R_RegisterCFinalizerEx(xptr, zip_raw_state_finalizer, TRUE);
    UNPROTECT(1);
    return xptr;
}

static ZipRawState* unwrap_state(SEXP data1) {
    auto* st = static_cast<ZipRawState*>(R_ExternalPtrAddr(data1));
    if (!st) Rf_error("ZipRawAltrep state has been released");
    return st;
}

// ---- Class methods ----

static R_xlen_t zip_raw_length(SEXP x) {
    SEXP d1 = R_altrep_data1(x);
    if (d1 == R_NilValue) return XLENGTH(R_altrep_data2(x));
    auto* st = unwrap_state(d1);
    if (st->slot->deactivated) return 0;
    return static_cast<R_xlen_t>(st->slot->length);
}

static void* zip_raw_dataptr(SEXP x, Rboolean writeable) {
    // Already materialized: just return the buffer.
    if (R_altrep_data1(x) == R_NilValue) {
        return RAW(R_altrep_data2(x));
    }
    SEXP d1 = R_altrep_data1(x);
    auto* st = unwrap_state(d1);
    if (st->slot->deactivated) {
        // Deactivated: return inert buffer (never written through; length=0).
        return const_cast<Rbyte*>(inert_buffer);
    }
    R_xlen_t n = static_cast<R_xlen_t>(st->slot->length);
    if (writeable) {
        if (st->slot->writable_inplace) {
            // Phase 7: writable in-place (reserve path). The mmap region is
            // PROT_READ | PROT_WRITE on writable opens, and the caller is
            // expected to fill the bytes here directly. const_cast satisfies
            // the void* signature; the underlying mapping is genuinely
            // writable.
            return const_cast<uint8_t*>(st->data);
        }
        // Default: materialize a private copy.
        SEXP m = PROTECT(Rf_allocVector(RAWSXP, n));
        if (n > 0) std::memcpy(RAW(m), st->data, static_cast<size_t>(n));
        R_set_altrep_data1(x, R_NilValue);
        R_set_altrep_data2(x, m);
        UNPROTECT(1);
        return RAW(m);
    }
    // SAFETY: writeable=FALSE — mmap is PROT_READ. Caller must not write
    // through the returned pointer. const_cast satisfies the void* signature.
    return const_cast<uint8_t*>(st->data);
}

static const void* zip_raw_dataptr_or_null(SEXP x) {
    SEXP d1 = R_altrep_data1(x);
    if (d1 == R_NilValue) return DATAPTR_RO(R_altrep_data2(x));
    auto* st = unwrap_state(d1);
    if (st->slot->deactivated) return inert_buffer;
    return st->data;
}

static Rbyte zip_raw_elt(SEXP x, R_xlen_t i) {
    SEXP d1 = R_altrep_data1(x);
    if (d1 == R_NilValue) return RAW(R_altrep_data2(x))[i];
    auto* st = unwrap_state(d1);
    if (st->slot->deactivated) return 0;
    return st->data[i];
}

static R_xlen_t zip_raw_get_region(SEXP x, R_xlen_t start, R_xlen_t size, Rbyte* buf) {
    R_xlen_t len = zip_raw_length(x);
    if (start < 0 || start >= len) return 0;
    R_xlen_t avail = len - start;
    R_xlen_t n = (size < avail) ? size : avail;
    if (n <= 0) return 0;
    SEXP d1 = R_altrep_data1(x);
    if (d1 == R_NilValue) {
        std::memcpy(buf, RAW(R_altrep_data2(x)) + start, static_cast<size_t>(n));
        return n;
    }
    auto* st = unwrap_state(d1);
    if (st->slot->deactivated) {
        // Should not happen: length is 0 when deactivated, so the bounds
        // check above already rejected. Defensive: zero-fill.
        std::memset(buf, 0, static_cast<size_t>(n));
        return n;
    }
    std::memcpy(buf, st->data + start, static_cast<size_t>(n));
    return n;
}

static Rboolean zip_raw_inspect(SEXP x, int /*pre*/, int /*deep*/, int /*pvec*/,
                                void (*/*inspect_subtree*/)(SEXP, int, int, int)) {
    SEXP d1 = R_altrep_data1(x);
    const char* state = "materialized";
    if (d1 != R_NilValue) {
        auto* st = unwrap_state(d1);
        state = st->slot->deactivated ? "deactivated" : "active";
    }
    Rprintf("dafr::ZipRawAltrep length=%lld [%s]\n",
            static_cast<long long>(zip_raw_length(x)), state);
    return TRUE;
}

// Serialize as an ordinary RAWSXP. Deactivated -> empty raw.
static SEXP zip_raw_serialized_state(SEXP x) {
    R_xlen_t n = zip_raw_length(x);
    SEXP out = PROTECT(Rf_allocVector(RAWSXP, n));
    if (n > 0) {
        SEXP d1 = R_altrep_data1(x);
        if (d1 == R_NilValue) {
            std::memcpy(RAW(out), RAW(R_altrep_data2(x)), static_cast<size_t>(n));
        } else {
            auto* st = unwrap_state(d1);
            // Length is 0 if deactivated, so we know the slot is active here.
            std::memcpy(RAW(out), st->data, static_cast<size_t>(n));
        }
    }
    UNPROTECT(1);
    return out;
}

static SEXP zip_raw_unserialize(SEXP /*cls*/, SEXP state) { return state; }

void init_altrep_zip_raw(DllInfo* dll) {
    ZipRawClass = R_make_altraw_class("ZipRawAltrep", "dafr", dll);
    R_set_altrep_Length_method(ZipRawClass, zip_raw_length);
    R_set_altrep_Inspect_method(ZipRawClass, zip_raw_inspect);
    R_set_altrep_Serialized_state_method(ZipRawClass, zip_raw_serialized_state);
    R_set_altrep_Unserialize_method(ZipRawClass, zip_raw_unserialize);
    R_set_altvec_Dataptr_method(ZipRawClass, zip_raw_dataptr);
    R_set_altvec_Dataptr_or_null_method(ZipRawClass, zip_raw_dataptr_or_null);
    R_set_altraw_Elt_method(ZipRawClass, zip_raw_elt);
    R_set_altraw_Get_region_method(ZipRawClass, zip_raw_get_region);
}

SEXP make_zip_raw_altrep(MmapZipStore* store, uint64_t offset, uint64_t length) {
    // Back-compat overload without xptr; will produce a vector that is NOT
    // anchored to the R-level store xptr. Callers that need close-on-GC
    // safety must use make_zip_raw_altrep_with_xptr.
    if (!store) Rf_error("make_zip_raw_altrep: null store");
    const uint8_t* base = store->file_mmap_base();
    if (!base) Rf_error("make_zip_raw_altrep: store has no mapped region");
    auto slot = store->register_altrep_slot(offset, length);
    const uint8_t* data = base + offset;
    SEXP state_xptr = PROTECT(wrap_state(std::move(slot), data, R_NilValue));
    SEXP out = R_new_altrep(ZipRawClass, state_xptr, R_NilValue);
    UNPROTECT(1);
    return out;
}

SEXP make_zip_raw_altrep_with_xptr(MmapZipStore* store, uint64_t offset,
                                   uint64_t length, SEXP store_xptr) {
    if (!store) Rf_error("make_zip_raw_altrep_with_xptr: null store");
    const uint8_t* base = store->file_mmap_base();
    if (!base) Rf_error("make_zip_raw_altrep_with_xptr: store has no mapped region");
    auto slot = store->register_altrep_slot(offset, length);
    const uint8_t* data = base + offset;
    SEXP state_xptr = PROTECT(wrap_state(std::move(slot), data, store_xptr));
    SEXP out = R_new_altrep(ZipRawClass, state_xptr, R_NilValue);
    UNPROTECT(1);
    return out;
}

SEXP make_zip_raw_altrep_writable(MmapZipStore* store, uint64_t offset,
                                  uint64_t length, SEXP store_xptr) {
    if (!store) Rf_error("make_zip_raw_altrep_writable: null store");
    const uint8_t* base = store->file_mmap_base();
    if (!base) Rf_error("make_zip_raw_altrep_writable: store has no mapped region");
    auto slot = store->register_altrep_slot_writable(offset, length);
    const uint8_t* data = base + offset;
    SEXP state_xptr = PROTECT(wrap_state(std::move(slot), data, store_xptr));
    SEXP out = R_new_altrep(ZipRawClass, state_xptr, R_NilValue);
    UNPROTECT(1);
    return out;
}

}  // namespace dafr
