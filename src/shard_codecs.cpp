// Inner-codec helpers for reading packed/sharded Zarr v3 ZarrDaf arrays.
//
//   - crc32c: ALWAYS compiled (validates the ZEP-0002 shard index).
//   - blosc / zstd decompress: compiled only when configure found the system
//     library (-DHAVE_BLOSC / -DHAVE_ZSTD); otherwise a stub raises an
//     actionable error so the flat path stays CRAN-clean.
//
// gzip inner chunks are decoded in R via memDecompress(); only blosc and zstd
// need a C backend. The only consumer is R/zarr_sharded.R.

#include <cpp11.hpp>
#include "crc32c.h"
#ifdef HAVE_BLOSC
  // Classic c-blosc (v1) exposes blosc.h + blosc_decompress; the modern c-blosc2
  // exposes blosc2.h + the always-available blosc1_* legacy API (which decodes
  // the blosc1 chunks DAF writes). configure picks via HAVE_BLOSC2.
  #ifdef HAVE_BLOSC2
    #include <blosc2.h>
    #define DAFR_BLOSC_DECOMPRESS    blosc1_decompress
    #define DAFR_BLOSC_CBUFFER_SIZES blosc1_cbuffer_sizes
  #else
    #include <blosc.h>
    #define DAFR_BLOSC_DECOMPRESS    blosc_decompress
    #define DAFR_BLOSC_CBUFFER_SIZES blosc_cbuffer_sizes
  #endif
#endif
#ifdef HAVE_ZSTD
#include <zstd.h>
#endif

[[cpp11::register]]
double dafr_crc32c_cpp(cpp11::raws x) {
    R_xlen_t n = x.size();
    const unsigned char* p = (n > 0)
        ? reinterpret_cast<const unsigned char*>(RAW(x.data())) : nullptr;
    return static_cast<double>(dafr_crc32c(p, static_cast<size_t>(n)));
}

// Decompress a classic blosc1 chunk. out_nbytes is the uncompressed size when
// the caller knows it (fixed-width dtype = n_elem * elem_size); pass <= 0 for
// variable-length (string/vlen-utf8) chunks, where the size is read from the
// blosc header.
[[cpp11::register]]
cpp11::raws dafr_blosc_decompress_cpp(cpp11::raws src, double out_nbytes) {
#ifdef HAVE_BLOSC
    size_t want;
    if (out_nbytes > 0) {
        want = static_cast<size_t>(out_nbytes);
    } else {
        size_t nbytes = 0, cbytes = 0, blocksize = 0;
        DAFR_BLOSC_CBUFFER_SIZES(reinterpret_cast<const void*>(RAW(src.data())),
                                 &nbytes, &cbytes, &blocksize);
        want = nbytes;
    }
    cpp11::writable::raws out(static_cast<R_xlen_t>(want));
    const void* s = reinterpret_cast<const void*>(RAW(src.data()));
    void* d = reinterpret_cast<void*>(RAW(out.data()));
    int got = DAFR_BLOSC_DECOMPRESS(s, d, want);
    if (got < 0) cpp11::stop("blosc_decompress failed (code %d)", got);
    if (static_cast<size_t>(got) != want)
        cpp11::stop("blosc_decompress size mismatch: got %d want %zu",
                    got, want);
    return out;
#else
    (void)src; (void)out_nbytes;
    cpp11::stop("Reading blosc-packed ZarrDaf requires c-blosc; install it "
                "(e.g. `conda install -c conda-forge c-blosc`) and reinstall "
                "dafr.");
#endif
}

// Decompress a raw zstd frame. out_nbytes is the uncompressed size when known;
// pass <= 0 for variable-length chunks, where the size is read from the frame
// header (ZSTD_getFrameContentSize).
[[cpp11::register]]
cpp11::raws dafr_zstd_decompress_cpp(cpp11::raws src, double out_nbytes) {
#ifdef HAVE_ZSTD
    size_t want;
    if (out_nbytes > 0) {
        want = static_cast<size_t>(out_nbytes);
    } else {
        unsigned long long sz = ZSTD_getFrameContentSize(
            reinterpret_cast<const void*>(RAW(src.data())),
            static_cast<size_t>(src.size()));
        if (sz == ZSTD_CONTENTSIZE_UNKNOWN || sz == ZSTD_CONTENTSIZE_ERROR)
            cpp11::stop("ZSTD frame content size unknown");
        want = static_cast<size_t>(sz);
    }
    cpp11::writable::raws out(static_cast<R_xlen_t>(want));
    const void* s = reinterpret_cast<const void*>(RAW(src.data()));
    void* d = reinterpret_cast<void*>(RAW(out.data()));
    size_t got = ZSTD_decompress(d, want, s, static_cast<size_t>(src.size()));
    if (ZSTD_isError(got))
        cpp11::stop("ZSTD_decompress failed: %s", ZSTD_getErrorName(got));
    if (got != want) cpp11::stop("ZSTD_decompress size mismatch");
    return out;
#else
    (void)src; (void)out_nbytes;
    cpp11::stop("Reading zstd-packed ZarrDaf requires libzstd; install it "
                "(e.g. `conda install -c conda-forge zstd`) and reinstall "
                "dafr.");
#endif
}

[[cpp11::register]] bool dafr_have_blosc_cpp() {
#ifdef HAVE_BLOSC
    return true;
#else
    return false;
#endif
}

[[cpp11::register]] bool dafr_have_zstd_cpp() {
#ifdef HAVE_ZSTD
    return true;
#else
    return false;
#endif
}
