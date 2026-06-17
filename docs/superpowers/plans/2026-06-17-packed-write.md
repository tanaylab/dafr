# Packed/Sharded WRITE Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Write full dual-format (`packed_format = "indexed+zipped"`) packed/sharded ZarrDaf and FilesDaf/ZipDaf stores from dafr, byte-compatible with DataAxesFormats.jl, defaulting to Julia's `blosc_zstd_bitshuffle` codec.

**Architecture:** A storage-agnostic shard *encoder* (`R/shard_encode.R` + `R/shard_zip.R`), the exact inverse of the existing read core (`R/zarr_sharded.R`), consumed by thin per-backend write integrations. Compression encoders mirror the existing decode wrappers in `src/shard_codecs.cpp` (gzip via base R; zstd/blosc via the same `configure` gate). Packed write is opt-in (`packed=FALSE` default); the flat path is untouched.

**Tech Stack:** R (S7, jsonlite, bit64, Matrix, base `memCompress`), cpp11/C (`src/crc32.c` zlib CRC-32; `#ifdef`-gated zstd + blosc encode), the existing autoconf `configure`, testthat. Verification fixtures: the committed Julia-written packed stores at `tests/testthat/fixtures/zpk/{gz,zs,bz,bl}.daf.zarr` plus regen via `dev/scripts/gen_packed_fixtures.jl` / `gen_files_packed_fixtures.jl`.

---

## Verified ground-truth (from the read port + repo inspection, 2026-06-17)

**Do not re-derive these; they are confirmed in the current tree.** Anything tagged
**[PIN]** is from the Julia format map and MUST be confirmed against a `zpk` fixture
in the first step of the task that uses it.

1. **Reusable encode helpers** (`R/zarr_v3.R`): `zarr_v3_encode_chunk(value, dtype)`
   -> raw LE bytes (float64/int32/int64/bool); `zarr_v3_encode_strings(strings)` ->
   vlen-utf8 `[N:u32][len:u32][utf8]...`; `zarr_v3_size_for_dtype(dtype)`;
   `zarr_v3_dtype_for_r(value)`; `zarr_v3_chunk_path(base, ndim)` = `c/0` / `c/0/0`;
   `zarr_v3_write_array(store, base, meta)`; `zarr_v3_array_meta(shape, dtype)`;
   `zarr_v3_consolidate_upsert(store, base)`.

2. **Read core to invert** (`R/zarr_sharded.R`): `.zarr_shard_index(shard, node, cfg)`,
   `.zarr_shard_grid(node, cfg)`, `.zarr_sharding_config(node)`, `.zarr_inner_decode`,
   `.zarr_inner_decompress`, `.shard_decode_vector(shard, node)`,
   `.shard_decode_matrix(shard, node)`, `.zarr_zero_vector(dtype, n)`. The index is
   `N*(offset:u64 LE, nbytes:u64 LE)` then a 4-byte crc32c over the `N*16` bytes;
   empty sentinel `0xFFFF...FF`. `node$codecs[[1]]$name == "sharding_indexed"`;
   `cfg$chunk_shape`, `cfg$codecs` (`bytes`/`vlen-utf8` + compressor),
   `cfg$index_codecs`, `cfg$index_location`.

3. **C decode wrappers to mirror** (`src/shard_codecs.cpp`): `dafr_crc32c_cpp(raws)`,
   `dafr_blosc_decompress_cpp(src, out_nbytes)`, `dafr_zstd_decompress_cpp(src,
   out_nbytes)`, `dafr_have_blosc_cpp()`, `dafr_have_zstd_cpp()`. Blosc decode uses
   `DAFR_BLOSC_DECOMPRESS` (`blosc1_decompress` or `blosc_decompress`); the encode
   side will use the matching `blosc1_compress`/`blosc_compress`.

4. **crc32c C core to mirror** (`src/crc32c.c`/`.h`): table-on-first-use, reflected,
   `extern "C"`-guarded header. The zlib CRC-32 needed for ZIP entries is the same
   shape with poly `0xEDB88320`.

5. **ZarrDaf write chokepoints** (`R/zarr_format.R`): `zarr_daf(uri, mode, name)` at
   L73; `.zarr_write_dense_array(store, base, values, shape)` at L314 (ALL dense:
   scalar/axis/dense-vector/dense-matrix funnel here); `.zarr_write_sparse_vector`
   at L744 (`nzind`/`nzval` components); `.zarr_write_sparse_matrix` at L1054
   (`colptr`/`rowval`/`nzval`); `.zarr_write_dense_matrix` at L1046 (reversed shape
   `c(nc, nr)`, `as.vector(mat)` column-major).

6. **FilesDaf write chokepoints**: `files_daf(path, mode, name)` at `R/files_daf.R:34`;
   `.write_bin_dense(path, value, dtype)` + `.write_descriptor_dense(path, dtype)` +
   `.write_descriptor_sparse(path, comps)` in `R/files_io.R`;
   `.files_write_vector_dense` (`R/files_daf_write.R:136`),
   `.files_write_vector_sparse_numeric` (L159), `.files_write_matrix_sparse` (L298),
   dense matrix set (L344-375). Naming: `<name>.data`/`.nzind`/`.nzval`/`.colptr`/
   `.rowval` + `<name>.json`.

7. **FilesDaf packed read descriptor fields** (`R/files_packed.R`): `.files_is_packed`
   keys on `desc$packed_format`; `.files_packed_node` reads `desc$eltype`,
   `desc$compression`, `desc$chunk_shape`; reader requires
   `packed_format == "indexed+zipped"`. Packed component files are `<name>.zip` /
   `<name>.<comp>.zip`.

8. **Threshold + chunk shape [PIN]**: pack iff `shape[1] * effective_sizeof(eltype)
   >= target_kb*1024` (8192 default); `effective_sizeof` = `sizeof` for fixed-width,
   `16` for strings. `n_chunk_rows = min(target_bytes %/% sizeof(eltype), shape[1])`;
   vector inner `(n_chunk_rows)`, matrix inner `(n_chunk_rows, 1)`. Grid linear order
   column-major. Confirm against `zpk/gz.daf.zarr/vectors/cell/score/zarr.json`
   (`codecs[[1]]$configuration$chunk_shape`) and the score fixture (1200 elems,
   inner 1024, N=2, first offset 89).

9. **Dev libs**: build with `./configure` in an env that has c-blosc + libzstd (conda
   `dafr-mcview`, or system R 4.4.1 + `BLOSC_HOME=<Blosc_jll artifact>`) so
   blosc/zstd encode tasks run; `gzip` runs on every build. Test invocation:
   `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/<file>")'`.

---

## File Structure

- **Create** `src/crc32.c` + `src/crc32.h` - zlib CRC-32 (poly `0xEDB88320`), always compiled.
- **Modify** `src/shard_codecs.cpp` - add `dafr_crc32_cpp`, `dafr_zstd_compress_cpp`, `dafr_blosc_compress_cpp`.
- **Modify** `R/cpp11.R` + `src/cpp11.cpp` - regenerated by `cpp11::cpp_register()`.
- **Create** `R/shard_encode.R` - threshold/chunk-shape math, serialize -> chunk -> compress, index builder, `.shard_assemble`.
- **Create** `R/shard_zip.R` - ZIP dual-format framing (local headers, central directory, ZIP64 EOCD, per-codec method, `codec.json`).
- **Create** `R/packed_opts.R` - `.packed_opts()` options resolver + codec/lib validation.
- **Modify** `R/zarr_v3.R` - add `zarr_v3_sharded_array_meta()`.
- **Modify** `R/zarr_format.R` - `packed=` arg; route dense + sparse component writes through the sharded path when packed + over threshold.
- **Modify** `R/files_io.R` - packed descriptor fields in `.write_descriptor_*`.
- **Modify** `R/files_daf.R` + `R/files_daf_write.R` - `packed=` arg; write `.zip` shards when packed + over threshold.
- **Create** tests: `test-crc32.R`, `test-shard-codecs-encode.R`, `test-shard-encode.R`, `test-shard-zip.R`, `test-zarr-packed-write.R`, `test-zarr-packed-write-interop.R`, `test-files-packed-write.R`, `test-files-packed-write-interop.R`.
- **Modify** `DESCRIPTION`, `NEWS.md`, `dev/parity-audit-2026-06-11/REMAINING-GAP.md`.

---

# Phase 1 - C encoders + zlib CRC-32

## Task 1: zlib CRC-32 C core + cpp11 wrapper

**Files:**
- Create: `src/crc32.h`, `src/crc32.c`
- Modify: `src/shard_codecs.cpp`, `R/cpp11.R`, `src/cpp11.cpp`
- Test: `tests/testthat/test-crc32.R`

- [ ] **Step 1: Write `src/crc32.h`.**

```c
#ifndef DAFR_CRC32_H
#define DAFR_CRC32_H
#include <stddef.h>
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif
/* Standard zlib CRC-32 (poly 0x04C11DB7, reflected 0xEDB88320), seed 0,
   finalized (~crc). Used for ZIP local-file-header / central-directory entry
   CRCs in dual-format packed shards (distinct from the crc32c shard index). */
uint32_t dafr_crc32(const unsigned char *buf, size_t len);
#ifdef __cplusplus
}
#endif
#endif
```

- [ ] **Step 2: Write `src/crc32.c`** (mirror of `src/crc32c.c`, different poly).

```c
#include "crc32.h"

static uint32_t TBL[256];
static int READY = 0;

static void init_table(void) {
    uint32_t n, c;
    int k;
    for (n = 0; n < 256; n++) {
        c = n;
        for (k = 0; k < 8; k++)
            c = (c & 1) ? (0xEDB88320u ^ (c >> 1)) : (c >> 1);
        TBL[n] = c;
    }
    READY = 1;
}

uint32_t dafr_crc32(const unsigned char *buf, size_t len) {
    uint32_t crc;
    size_t i;
    if (!READY) init_table();
    crc = 0xFFFFFFFFu;
    for (i = 0; i < len; i++)
        crc = TBL[(crc ^ buf[i]) & 0xFFu] ^ (crc >> 8);
    return crc ^ 0xFFFFFFFFu;
}
```

- [ ] **Step 3: Add the cpp11 wrapper** to `src/shard_codecs.cpp` (after the `#include "crc32.h"` which you also add next to `#include "crc32c.h"`):

```cpp
[[cpp11::register]]
double dafr_crc32_cpp(cpp11::raws x) {
    R_xlen_t n = x.size();
    const unsigned char* p = (n > 0)
        ? reinterpret_cast<const unsigned char*>(RAW(x.data())) : nullptr;
    return static_cast<double>(dafr_crc32(p, static_cast<size_t>(n)));
}
```

- [ ] **Step 4: Regenerate cpp11 bindings.**

Run: `Rscript -e 'cpp11::cpp_register()'`
Expected: `R/cpp11.R` + `src/cpp11.cpp` gain `dafr_crc32_cpp`.

- [ ] **Step 5: Write the failing test** `tests/testthat/test-crc32.R`:

```r
test_that("zlib crc32 matches the canonical check value", {
    # crc32("123456789") == 0xCBF43926 (zlib check value).
    expect_equal(dafr:::dafr_crc32_cpp(charToRaw("123456789")), 0xCBF43926)
})

test_that("zlib crc32 of empty input is 0", {
    expect_equal(dafr:::dafr_crc32_cpp(raw(0L)), 0)
})
```

- [ ] **Step 6: Build + run.**

Run: `Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-crc32.R")'`
Expected: 2 pass.

- [ ] **Step 7: Commit.**

```bash
git add src/crc32.h src/crc32.c src/shard_codecs.cpp R/cpp11.R src/cpp11.cpp tests/testthat/test-crc32.R
git commit -m "feat(packed-write): always-compiled zlib CRC-32 C core for ZIP framing"
```

## Task 2: zstd + blosc compress C wrappers (configure-gated)

**Files:**
- Modify: `src/shard_codecs.cpp`, `R/cpp11.R`, `src/cpp11.cpp`
- Test: `tests/testthat/test-shard-codecs-encode.R`

- [ ] **Step 1: Add `dafr_zstd_compress_cpp`** to `src/shard_codecs.cpp` (gated like the decode side):

```cpp
// Compress a raw byte buffer to a single zstd frame at `level`.
[[cpp11::register]]
cpp11::raws dafr_zstd_compress_cpp(cpp11::raws src, int level) {
#ifdef HAVE_ZSTD
    size_t in_n = static_cast<size_t>(src.size());
    size_t bound = ZSTD_compressBound(in_n);
    cpp11::writable::raws out(static_cast<R_xlen_t>(bound));
    const void* s = (in_n > 0)
        ? reinterpret_cast<const void*>(RAW(src.data())) : nullptr;
    void* d = reinterpret_cast<void*>(RAW(out.data()));
    size_t got = ZSTD_compress(d, bound, s, in_n, level);
    if (ZSTD_isError(got))
        cpp11::stop("ZSTD_compress failed: %s", ZSTD_getErrorName(got));
    out.resize(static_cast<R_xlen_t>(got));
    return out;
#else
    (void)src; (void)level;
    cpp11::stop("Writing zstd-packed stores requires libzstd; install it "
                "(e.g. `conda install -c conda-forge zstd`) and reinstall dafr.");
#endif
}
```

- [ ] **Step 2: Add `dafr_blosc_compress_cpp`** (classic blosc1 chunk; `doshuffle`: 0=none, 1=byteshuffle, 2=bitshuffle):

```cpp
// Compress to a classic blosc1 chunk: cname selects the sub-codec ("zstd"/"lz4"),
// doshuffle the filter (2 = bitshuffle), typesize the element width for shuffle.
[[cpp11::register]]
cpp11::raws dafr_blosc_compress_cpp(cpp11::raws src, int level,
                                    std::string cname, int doshuffle,
                                    int typesize) {
#ifdef HAVE_BLOSC
    size_t in_n = static_cast<size_t>(src.size());
    size_t bound = in_n + BLOSC_MAX_OVERHEAD;
    cpp11::writable::raws out(static_cast<R_xlen_t>(bound));
    const void* s = (in_n > 0)
        ? reinterpret_cast<const void*>(RAW(src.data())) : nullptr;
    void* d = reinterpret_cast<void*>(RAW(out.data()));
    int got = DAFR_BLOSC_COMPRESS(level, doshuffle, static_cast<size_t>(typesize),
                                  in_n, s, d, bound, cname.c_str(), 0, 1);
    if (got <= 0) cpp11::stop("blosc compress failed (code %d)", got);
    out.resize(static_cast<R_xlen_t>(got));
    return out;
#else
    (void)src; (void)level; (void)cname; (void)doshuffle; (void)typesize;
    cpp11::stop("Writing blosc-packed stores requires c-blosc; install it "
                "(e.g. `conda install -c conda-forge c-blosc`) and reinstall dafr.");
#endif
}
```

Add the compress macro next to the existing `DAFR_BLOSC_DECOMPRESS` defines in the
`#ifdef HAVE_BLOSC` block (both blosc1 and blosc2 expose `blosc1_compress` /
`blosc_compress_ctx`; use the context API for thread safety):

```cpp
  // (inside the existing HAVE_BLOSC block, alongside DAFR_BLOSC_DECOMPRESS)
  #ifdef HAVE_BLOSC2
    #define DAFR_BLOSC_COMPRESS blosc1_compress_ctx
  #else
    #define DAFR_BLOSC_COMPRESS blosc_compress_ctx
  #endif
```

> **Step 2 note:** `blosc_compress_ctx`/`blosc1_compress_ctx` signature is
> `(clevel, doshuffle, typesize, nbytes, src, dest, destsize, compressor,
> blocksize, numinternalthreads)`. Confirm the exact symbol available in the
> linked c-blosc with `Rscript -e 'dafr:::dafr_have_blosc_cpp()'` after build; if
> the context API is absent in classic c-blosc, fall back to
> `blosc_set_compressor(cname.c_str())` + `blosc_compress(level, doshuffle,
> typesize, in_n, s, d, bound)`.

- [ ] **Step 3: Regenerate + build.**

Run: `Rscript -e 'cpp11::cpp_register()'`
Run: `./configure && Rscript -e 'pkgload::load_all(".")'`

- [ ] **Step 4: Write the failing test** `tests/testthat/test-shard-codecs-encode.R` (encode then decode via the existing wrappers == identity):

```r
test_that("zstd compress round-trips through the decoder", {
    skip_if_not(dafr:::dafr_have_zstd_cpp(), "libzstd not built in")
    x <- writeBin(as.double(1:1024), raw(), size = 8L, endian = "little")
    comp <- dafr:::dafr_zstd_compress_cpp(x, 5L)
    expect_true(length(comp) < length(x))
    back <- dafr:::dafr_zstd_decompress_cpp(comp, length(x))
    expect_identical(back, x)
})

test_that("blosc bitshuffle compress round-trips through the decoder", {
    skip_if_not(dafr:::dafr_have_blosc_cpp(), "c-blosc not built in")
    x <- writeBin(as.double(1:1024), raw(), size = 8L, endian = "little")
    comp <- dafr:::dafr_blosc_compress_cpp(x, 5L, "zstd", 2L, 8L)
    back <- dafr:::dafr_blosc_decompress_cpp(comp, length(x))
    expect_identical(back, x)
})
```

- [ ] **Step 5: Run.**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-shard-codecs-encode.R")'`
Expected: both pass (in the c-blosc/zstd env) or skip (stock build).

- [ ] **Step 6: Commit.**

```bash
git add src/shard_codecs.cpp R/cpp11.R src/cpp11.cpp tests/testthat/test-shard-codecs-encode.R
git commit -m "feat(packed-write): configure-gated zstd + blosc compress wrappers"
```

---

# Phase 2 - Shard encode core (gzip, Zarr-index-only blob)

## Task 3: Threshold + inner-chunk-shape helpers

**Files:**
- Create: `R/shard_encode.R`
- Test: `tests/testthat/test-shard-encode.R`

- [ ] **Step 1: Confirm the [PIN] threshold/chunk-shape** against the fixture.

Run: `Rscript -e 'cat(readLines("tests/testthat/fixtures/zpk/gz.daf.zarr/vectors/cell/score/zarr.json"))'`
Expected: `codecs[0].name == "sharding_indexed"`, its `configuration.chunk_shape == [1024]`, outer `shape == [1200]`. (Score is Float64, 1200 elems: `1200*8 = 9600 >= 8192` -> packed; `8192 %/% 8 = 1024` inner.) Record any deviation before coding.

- [ ] **Step 2: Failing test** in `tests/testthat/test-shard-encode.R`:

```r
test_that(".shard_effective_sizeof and threshold match Julia", {
    expect_equal(dafr:::.shard_effective_sizeof("float64"), 8L)
    expect_equal(dafr:::.shard_effective_sizeof("string"), 16L)
    # 1200 float64 = 9600 B >= 8192 -> pack
    expect_true(dafr:::.shard_should_pack(1200L, "float64", 8L))
    # 1000 float64 = 8000 B < 8192 -> flat
    expect_false(dafr:::.shard_should_pack(1000L, "float64", 8L))
})

test_that(".shard_inner_chunk_shape gives column-slab chunks", {
    expect_equal(dafr:::.shard_inner_chunk_shape(c(1200L), "float64", 8L), 1024L)
    expect_equal(dafr:::.shard_inner_chunk_shape(c(1200L, 8L), "float64", 8L),
                 c(1024L, 1L))
})
```

- [ ] **Step 3: Run - expect FAIL** (functions not found).

- [ ] **Step 4: Implement** the header + helpers in `R/shard_encode.R`:

```r
#' @include zarr_sharded.R
NULL

# R/shard_encode.R
# Dual-format packed/sharded WRITE encoder - the exact inverse of the read core
# in R/zarr_sharded.R. Pure R except inner-chunk compression (gzip via base-R
# memCompress; zstd/blosc via the configure-gated wrappers in
# src/shard_codecs.cpp). Storage-agnostic: produces one shard blob (raw vector);
# the ZarrDaf / FilesDaf writers persist it at the single outer-chunk key /
# `<name>.zip`. ZIP framing lives in R/shard_zip.R.

# Estimated element size for chunk-sizing: real width for fixed-width dtypes, a
# 16-byte estimate for strings (matches Julia STRING_SIZEOF_ESTIMATE).
.shard_effective_sizeof <- function(dtype) {
    if (identical(dtype, "string")) return(16L)
    zarr_v3_size_for_dtype(dtype)
}

# TRUE if a component's first-dim byte size meets the pack threshold.
.shard_should_pack <- function(dim1, dtype, target_kb) {
    as.numeric(dim1) * .shard_effective_sizeof(dtype) >= target_kb * 1024
}

# Inner chunk shape: n_chunk_rows over dim1 (column-slab for matrices).
.shard_inner_chunk_shape <- function(shape, dtype, target_kb) {
    target_bytes <- target_kb * 1024L
    esz <- .shard_effective_sizeof(dtype)
    n_rows <- min(target_bytes %/% esz, shape[[1L]])
    if (length(shape) == 1L) as.integer(n_rows) else c(as.integer(n_rows), 1L)
}
```

- [ ] **Step 5: Run - expect PASS.**

- [ ] **Step 6: Commit.**

```bash
git add R/shard_encode.R tests/testthat/test-shard-encode.R
git commit -m "feat(packed-write): threshold + inner-chunk-shape helpers"
```

## Task 4: Inner-chunk compress dispatch (inverse of `.zarr_inner_decompress`)

**Files:**
- Modify: `R/shard_encode.R`
- Test: `tests/testthat/test-shard-encode.R`

- [ ] **Step 1: Failing test** (compress then decode via the read core == identity, for every available codec):

```r
test_that(".shard_inner_compress inverts .zarr_inner_decompress (gzip)", {
    cfg_gzip <- list(codecs = list(list(name = "bytes"), list(name = "gzip")))
    raw_bytes <- writeBin(as.double(1:1024), raw(), size = 8L, endian = "little")
    comp <- dafr:::.shard_inner_compress(raw_bytes, cfg_gzip, level = 5L)
    back <- dafr:::.zarr_inner_decompress(comp, cfg_gzip, out_nbytes = length(raw_bytes))
    expect_identical(back, raw_bytes)
})
```

- [ ] **Step 2: Run - expect FAIL.**

- [ ] **Step 3: Implement** (append to `R/shard_encode.R`). gzip via `memCompress`;
the gzip ZIP-method-8 re-framing is handled later in `R/shard_zip.R` - here the
inner chunk is the full gzip stream (what the Zarr gzip codec / `memDecompress`
reads). zstd/blosc through the C wrappers:

```r
# The compressor name in the inner pipeline (skip the array->bytes step).
.shard_inner_compressor <- function(cfg) {
    for (c in cfg$codecs) {
        if (!c$name %in% c("bytes", "vlen-utf8")) return(c$name)
    }
    "none"
}

# Map a DAF compression symbol to (compressor name, blosc cname). The compressor
# name is what the read core's .zarr_inner_compressor() returns.
.SHARD_CODEC_TABLE <- list(
    blosc_zstd_bitshuffle = list(compressor = "blosc", cname = "zstd"),
    blosc_lz4_bitshuffle  = list(compressor = "blosc", cname = "lz4"),
    zstd                  = list(compressor = "zstd",  cname = NA_character_),
    gzip                  = list(compressor = "gzip",  cname = NA_character_)
)

# Compress one inner chunk's raw element bytes per the cfg's inner compressor.
# `typesize` is the element width (for blosc bitshuffle); `level` the clevel.
.shard_inner_compress <- function(raw_bytes, cfg, level, typesize = 1L) {
    comp <- .shard_inner_compressor(cfg)
    switch(comp,
        "none"  = raw_bytes,
        "gzip"  = memCompress(raw_bytes, type = "gzip"),
        "zstd"  = dafr_zstd_compress_cpp(raw_bytes, as.integer(level)),
        "blosc" = {
            cname <- cfg$.blosc_cname %||% "zstd"
            dafr_blosc_compress_cpp(raw_bytes, as.integer(level), cname,
                                    2L, as.integer(typesize))  # 2 = bitshuffle
        },
        stop(sprintf("shard_encode: unsupported compressor %s", sQuote(comp)),
             call. = FALSE))
}
```

- [ ] **Step 4: Run - expect PASS** (gzip everywhere; add zstd/blosc cfg variants guarded by `skip_if_not` as in Task 2 if desired).

- [ ] **Step 5: Commit.**

```bash
git add R/shard_encode.R tests/testthat/test-shard-encode.R
git commit -m "feat(packed-write): inner-chunk compress dispatch (gzip/zstd/blosc)"
```

## Task 5: Chunk split + fill-pad + shard index + plain assemble (no ZIP)

**Files:**
- Modify: `R/shard_encode.R`
- Test: `tests/testthat/test-shard-encode.R`

- [ ] **Step 1: Failing test** - a Zarr-index-only blob (index + concatenated chunks, no ZIP yet) round-trips through the EXISTING reader `.shard_decode_vector` / `.shard_decode_matrix`:

```r
test_that("plain shard blob round-trips a vector through the read core", {
    vals <- as.numeric(1:1200)
    blob <- dafr:::.shard_assemble_plain(vals, "float64", shape = 1200L,
                                         inner = 1024L, codec = "gzip", level = 5L)
    node <- dafr:::.files_packed_node(
        list(eltype = "Float64", compression = "gzip", chunk_shape = list(1024L)),
        shape = 1200L, chunk_shape = 1024L)
    expect_equal(dafr:::.shard_decode_vector(blob, node), vals)
})

test_that("plain shard blob round-trips a matrix through the read core", {
    m <- matrix(as.numeric(1:(1200 * 8)), nrow = 1200, ncol = 8)
    # On-disk reversed shape [ncol, nrow]; inner column-slab [1, 1024] on disk.
    blob <- dafr:::.shard_assemble_plain(as.vector(m), "float64",
                                         shape = c(8L, 1200L), inner = c(1L, 1024L),
                                         codec = "gzip", level = 5L)
    node <- dafr:::.files_packed_node(
        list(eltype = "Float64", compression = "gzip",
             chunk_shape = list(1L, 1024L)),
        shape = c(8L, 1200L), chunk_shape = c(1L, 1024L))
    expect_equal(as.numeric(dafr:::.shard_decode_matrix(blob, node)), as.numeric(m))
})
```

> The reader treats the index offsets as absolute into the blob. A plain blob
> (index then chunks back-to-back) satisfies it because the offsets point at the
> chunk bytes directly. Task 6 inserts ZIP local headers and recomputes offsets;
> the same round-trip test must keep passing.

- [ ] **Step 2: Run - expect FAIL.**

- [ ] **Step 3: Implement** (append). Split the column-major element stream into the
inner-chunk grid (column-major linear order), fill-pad the partial chunk,
compress each, then lay out `[index][chunk0][chunk1]...` and build the start
index. The grid/order MUST match `.zarr_shard_grid` + the reader's tiling:

```r
# Split a flat (column-major) element vector into inner chunks in the grid's
# column-major linear order, fill-padding the final partial chunk per dim to the
# full inner shape. Returns a list of per-chunk element vectors.
.shard_split_chunks <- function(values, shape, inner) {
    grid <- list(outer = as.integer(shape), inner = as.integer(inner),
                 per_dim = as.integer(ceiling(shape / inner)))
    fill <- if (is.character(values)) "" else as(0, typeof(values))
    if (length(shape) == 1L) {
        chunks <- vector("list", grid$per_dim[[1L]])
        for (k in seq_len(grid$per_dim[[1L]])) {
            lo <- (k - 1L) * inner[[1L]]
            valid <- min(inner[[1L]], shape[[1L]] - lo)
            piece <- values[(lo + 1L):(lo + valid)]
            if (valid < inner[[1L]]) piece <- c(piece, rep(fill, inner[[1L]] - valid))
            chunks[[k]] <- piece
        }
        return(chunks)
    }
    # 2-D: on-disk column-major buffer dim=[d0,d1]; inner [i0,i1]; column-major
    # grid order (c0 fastest), each inner chunk emitted C-order over [i0,i1] to
    # match .shard_decode_matrix's local (a,b) at a*i1+b.
    d0 <- shape[[1L]]; d1 <- shape[[2L]]; i0 <- inner[[1L]]; i1 <- inner[[2L]]
    n0 <- grid$per_dim[[1L]]; n1 <- grid$per_dim[[2L]]
    buf <- values  # length d0*d1, on-disk C-order over [d0,d1]
    chunks <- vector("list", n0 * n1)
    lin <- 0L
    for (c0 in seq_len(n0)) for (c1 in seq_len(n1)) {  # column-major grid
        lin <- lin + 1L
        lo0 <- (c0 - 1L) * i0; lo1 <- (c1 - 1L) * i1
        v0 <- min(i0, d0 - lo0); v1 <- min(i1, d1 - lo1)
        piece <- rep(fill, i0 * i1)
        for (a in seq_len(v0)) {
            dst <- (a - 1L) * i1
            src <- (lo0 + a - 1L) * d1 + lo1
            piece[(dst + 1L):(dst + v1)] <- buf[(src + 1L):(src + v1)]
        }
        chunks[[lin]] <- piece
    }
    chunks
}

# Build the start-located shard index from per-chunk (offset, nbytes): N*16 LE
# bytes (offset:u64, nbytes:u64) then crc32c over them. Offsets are absolute
# into the final blob.
.shard_build_index <- function(offsets, nbytes) {
    con <- rawConnection(raw(0L), "wb"); on.exit(close(con))
    for (i in seq_along(offsets)) {
        .shard_write_u64(con, offsets[[i]]); .shard_write_u64(con, nbytes[[i]])
    }
    idx <- rawConnectionValue(con)
    crc <- dafr_crc32c_cpp(idx) %% 2^32
    c(idx, .shard_u32_raw(crc))
}

# Write a u64 (value < 2^53) as 8 LE bytes via lo/hi u32 halves.
.shard_write_u64 <- function(con, x) {
    lo <- x %% 2^32; hi <- (x - lo) / 2^32
    writeBin(.shard_u32_raw(lo), con); writeBin(.shard_u32_raw(hi), con)
}
.shard_u32_raw <- function(x) {
    as.raw(c(x %% 256, (x %/% 256) %% 256, (x %/% 65536) %% 256,
             (x %/% 16777216) %% 256))
}

# Assemble a PLAIN (no ZIP framing) shard blob: serialize -> chunk -> compress ->
# [index][chunk bytes...]. Used as the Phase-2 correctness pin; Task 6 swaps the
# layout for the ZIP dual-format one.
.shard_assemble_plain <- function(values, dtype, shape, inner, codec, level,
                                  cname = NULL) {
    cfg <- list(codecs = list(list(name = "bytes"),
                              list(name = .SHARD_CODEC_TABLE[[codec]]$compressor)),
                .blosc_cname = cname %||% .SHARD_CODEC_TABLE[[codec]]$cname)
    typesize <- if (identical(dtype, "string")) 1L else zarr_v3_size_for_dtype(dtype)
    chunks <- .shard_split_chunks(values, shape, inner)
    comp <- lapply(chunks, function(ch) {
        raw_bytes <- if (identical(dtype, "string"))
            zarr_v3_encode_strings(ch) else zarr_v3_encode_chunk(ch, dtype)
        .shard_inner_compress(raw_bytes, cfg, level, typesize)
    })
    n <- length(comp)
    nbytes <- vapply(comp, length, integer(1L))
    idx_size <- n * 16L + 4L
    offsets <- idx_size + c(0L, cumsum(nbytes)[-n])
    c(.shard_build_index(offsets, nbytes), do.call(c, comp))
}
```

- [ ] **Step 4: Run - expect PASS** (both vector and matrix round-trip through the read core).

- [ ] **Step 5: Commit.**

```bash
git add R/shard_encode.R tests/testthat/test-shard-encode.R
git commit -m "feat(packed-write): chunk split/pad + shard index + plain assemble"
```

---

# Phase 3 - ZIP dual-format framing

## Task 6: STORED-method framing (blosc default) + central directory + ZIP64 EOCD + codec.json

**Files:**
- Create: `R/shard_zip.R`
- Modify: `R/shard_encode.R` (`.shard_assemble` -> ZIP layout)
- Test: `tests/testthat/test-shard-zip.R`

- [ ] **Step 1: Confirm framing [PIN] against a Julia fixture.** Inspect a STORED
(blosc) shard's bytes and central directory:

Run: `Rscript -e 'b <- readBin("tests/testthat/fixtures/zpk/bz.daf.zarr/vectors/cell/score/c/0", "raw", n=1e6); cat(length(b), "\n"); cat(sprintf("%02x ", utils::head(b, 64)))'`
Then: `Rscript -e 'print(zip::zip_list("tests/testthat/fixtures/zpk/bz.daf.zarr/vectors/cell/score/c/0"))'` (if `zip::zip_list` reads a non-`.zip` extension, copy to a tempfile with `.zip` first).
Record: entry names (`c/0`, `c/1`, `codec.json`), methods (0), the offset of the first local header (= index size 36), and the EOCD region layout. Adjust constants below to match.

- [ ] **Step 2: Failing test** `tests/testthat/test-shard-zip.R`:

```r
test_that("a dual-format blosc shard is a legal ZIP and round-trips", {
    skip_if_not(dafr:::dafr_have_blosc_cpp(), "c-blosc not built in")
    vals <- as.numeric(1:1200)
    blob <- dafr:::.shard_assemble(vals, "float64", shape = 1200L, inner = 1024L,
                                   codec = "blosc_zstd_bitshuffle", level = 5L)
    # (a) Zarr index still resolves -> read core returns the values.
    node <- dafr:::.files_packed_node(
        list(eltype = "Float64", compression = "blosc_zstd_bitshuffle",
             chunk_shape = list(1024L)), shape = 1200L, chunk_shape = 1024L)
    expect_equal(dafr:::.shard_decode_vector(blob, node), vals)
    # (b) it is a valid ZIP with the expected entries.
    tmp <- tempfile(fileext = ".zip"); writeBin(blob, tmp)
    z <- zip::zip_list(tmp)
    expect_true(all(c("c/0", "c/1", "codec.json") %in% z$filename))
})
```

- [ ] **Step 3: Run - expect FAIL** (`.shard_assemble` not found).

- [ ] **Step 4: Implement `R/shard_zip.R`** - per-codec method, fixed-width names,
local headers, central directory, ZIP64 EOCD region, codec.json STORED entry. Use
the byte layouts from the design spec §7 (verified in Step 1):

```r
#' @include shard_encode.R
NULL

# R/shard_zip.R
# ZIP dual-format framing for packed shards (the byte-exact ZIP half of the
# "indexed+zipped" blob). The Zarr shard index (R/shard_encode.R) sits at byte 0;
# this module interleaves ZIP local file headers before each chunk, appends a
# fixed-width-named central directory + ZIP64 end-of-central-directory region, and
# (for STORED codecs) a `codec.json` entry recording the inner pipeline.

.ZIP_LFH_SIG <- 0x04034b50; .ZIP_CDE_SIG <- 0x02014b50
.ZIP64_EOCD_SIG <- 0x06064b50; .ZIP64_LOC_SIG <- 0x07064b50; .ZIP_EOCD_SIG <- 0x06054b50

# ZIP compression method for a DAF codec symbol [PIN: confirm in Task 6 Step 1].
.shard_zip_method <- function(codec) {
    switch(codec, zstd = 93L, gzip = 8L, 0L)  # blosc* / default -> STORED
}

# Fixed-width inner-chunk entry name `c/<i_N>/.../<i_1>` (reversed, zero-padded to
# ndigits(per_dim-1)). per_dim is the inner-chunk count per on-disk dimension.
.shard_chunk_name <- function(lin, per_dim) {
    coords <- arrayInd(lin, per_dim) - 1L          # column-major linear -> coords
    widths <- pmax(1L, nchar(as.character(per_dim - 1L)))
    rev_parts <- rev(formatC(coords, width = widths, flag = "0"))
    paste0("c/", paste(rev_parts, collapse = "/"))
}

# LE byte writers (raw vectors). u64 holds values < 2^53 exactly.
.shard_u16_raw <- function(x) as.raw(c(x %% 256, (x %/% 256) %% 256))
.shard_u64_raw <- function(x) {
    lo <- x %% 2^32; hi <- (x - lo) / 2^32
    c(.shard_u32_raw(lo), .shard_u32_raw(hi))
}
.U32MAX <- as.raw(c(0xff, 0xff, 0xff, 0xff)); .U16MAX <- as.raw(c(0xff, 0xff))

# Local file header [PIN: confirm version-needed / ZIP64-vs-inline per method in
# Task 6 Step 1]. STORED/zstd use ZIP64 sentinels + a 20-byte extra.
.shard_zip_local_header <- function(name, crc, csize, usize, method = 0L) {
    nm <- charToRaw(name)
    zip64 <- c(.shard_u16_raw(1L), .shard_u16_raw(16L),
               .shard_u64_raw(usize), .shard_u64_raw(csize))
    c(.shard_u32_raw(.ZIP_LFH_SIG), .shard_u16_raw(45L), .shard_u16_raw(0x0800L),
      .shard_u16_raw(method), .shard_u16_raw(0L), .shard_u16_raw(0x0021L),
      .shard_u32_raw(crc), .U32MAX, .U32MAX,
      .shard_u16_raw(length(nm)), .shard_u16_raw(length(zip64)), nm, zip64)
}

# Central directory entry [PIN] (46-byte fixed prefix + 28-byte ZIP64 extra).
.shard_zip_central_entry <- function(name, crc, csize, usize, lfh_off,
                                     method = 0L) {
    nm <- charToRaw(name)
    zip64 <- c(.shard_u16_raw(1L), .shard_u16_raw(24L),
               .shard_u64_raw(usize), .shard_u64_raw(csize),
               .shard_u64_raw(lfh_off))
    c(.shard_u32_raw(.ZIP_CDE_SIG), .shard_u16_raw(0x031eL), .shard_u16_raw(45L),
      .shard_u16_raw(0x0800L), .shard_u16_raw(method), .shard_u16_raw(0L),
      .shard_u16_raw(0x0021L), .shard_u32_raw(crc), .U32MAX, .U32MAX,
      .shard_u16_raw(length(nm)), .shard_u16_raw(length(zip64)),
      .shard_u16_raw(0L), .shard_u16_raw(0L), .shard_u16_raw(0L),
      .shard_u32_raw(33188 * 65536),  # 0o100644 << 16 (Unix perms, u32)
      .U32MAX, nm, zip64)
}

# ZIP64 EOCD record (56) + locator (20) + legacy EOCD (22) = 98 bytes [PIN].
.shard_zip_eocd <- function(n_entries, cd_off, cd_size) {
    z64 <- c(.shard_u32_raw(.ZIP64_EOCD_SIG), .shard_u64_raw(44),
             .shard_u16_raw(0x031eL), .shard_u16_raw(45L),
             .shard_u32_raw(0), .shard_u32_raw(0),
             .shard_u64_raw(n_entries), .shard_u64_raw(n_entries),
             .shard_u64_raw(cd_size), .shard_u64_raw(cd_off))
    loc <- c(.shard_u32_raw(.ZIP64_LOC_SIG), .shard_u32_raw(0),
             .shard_u64_raw(cd_off + cd_size), .shard_u32_raw(1))
    eocd <- c(.shard_u32_raw(.ZIP_EOCD_SIG), .shard_u16_raw(0L),
              .shard_u16_raw(0L), .U16MAX, .U16MAX, .U32MAX, .U32MAX,
              .shard_u16_raw(0L))
    c(z64, loc, eocd)
}

# codec.json STORED entry (records the inner pipeline) [PIN: exact JSON].
.shard_codec_json_entry <- function(cfg, lfh_off) {
    json <- charToRaw(as.character(jsonlite::toJSON(cfg$codecs, auto_unbox = TRUE)))
    crc <- dafr_crc32_cpp(json) %% 2^32
    lfh <- .shard_zip_local_header("codec.json", crc, length(json), length(json))
    list(body = c(lfh, json),
         central = .shard_zip_central_entry("codec.json", crc, length(json),
                                            length(json), lfh_off))
}

# Non-ZIP64 local header for DEFLATE/gzip entries [PIN]; the 10-byte gzip header
# is the `name` and sizes are inline (small chunks). Used by Task 8.
.shard_zip_local_header_raw <- function(name, crc, csize, usize, method) {
    nm <- name  # already raw (10-byte gzip header) for the gzip case
    c(.shard_u32_raw(.ZIP_LFH_SIG), .shard_u16_raw(20L), .shard_u16_raw(0x0800L),
      .shard_u16_raw(method), .shard_u16_raw(0L), .shard_u16_raw(0x0021L),
      .shard_u32_raw(crc), .shard_u32_raw(csize), .shard_u32_raw(usize),
      .shard_u16_raw(length(nm)), .shard_u16_raw(0L), nm)
}

# 10-byte gzip header for chunk k as a ZIP name field [PIN: confirm base64 of
# (k-1) in 3 bytes against zpk/gz.daf.zarr in Task 8 Step 1].
.shard_gzip_name <- function(k) {
    idx <- k - 1L
    three <- as.raw(c((idx %/% 65536) %% 256, (idx %/% 256) %% 256, idx %% 256))
    b64 <- charToRaw(jsonlite::base64_enc(three))   # 4 chars
    c(as.raw(c(0x1f, 0x8b, 0x08, 0x01)), b64, as.raw(c(0x02, 0xff)))
}
```

- [ ] **Step 5: Replace `.shard_assemble`** in `R/shard_encode.R` so it lays out the
ZIP framing and computes index offsets that point at each chunk's **data start**
(for STORED/zstd) - i.e. after that chunk's local file header:

```r
# Assemble the full dual-format shard blob: [index][LFH+chunk]*N
# [codec.json STORED entry if STORED][central directory][ZIP64 EOCD region].
# Index offsets point at each chunk's ZIP entry data (STORED/zstd) so the Zarr
# read core resolves them directly.
.shard_assemble <- function(values, dtype, shape, inner, codec, level,
                            cname = NULL) {
    cfg <- list(codecs = list(list(name = "bytes"),
                              list(name = .SHARD_CODEC_TABLE[[codec]]$compressor)),
                .blosc_cname = cname %||% .SHARD_CODEC_TABLE[[codec]]$cname)
    typesize <- if (identical(dtype, "string")) 1L else zarr_v3_size_for_dtype(dtype)
    per_dim <- as.integer(ceiling(shape / inner))
    chunks <- .shard_split_chunks(values, shape, inner)
    n <- length(chunks)
    idx_size <- n * 16L + 4L
    method <- .shard_zip_method(codec)
    # Build each chunk's compressed bytes + LFH; track index offsets.
    bodies <- vector("list", n); offsets <- numeric(n); nbytes <- numeric(n)
    centrals <- vector("list", n); cursor <- idx_size
    for (k in seq_len(n)) {
        plain <- if (identical(dtype, "string"))
            zarr_v3_encode_strings(chunks[[k]]) else
            zarr_v3_encode_chunk(chunks[[k]], dtype)
        comp <- .shard_inner_compress(plain, cfg, level, typesize)
        name <- .shard_chunk_name(k, per_dim)
        crc <- dafr_crc32_cpp(plain) %% 2^32
        lfh <- .shard_zip_local_header(name, crc, length(comp), length(plain))
        data_off <- cursor + length(lfh)
        offsets[[k]] <- data_off; nbytes[[k]] <- length(comp)  # STORED/zstd
        centrals[[k]] <- .shard_zip_central_entry(name, crc, length(comp),
                                                  length(plain), cursor)
        bodies[[k]] <- c(lfh, comp)
        cursor <- cursor + length(lfh) + length(comp)
    }
    # codec.json STORED entry for STORED codecs.
    extra <- NULL
    if (method == 0L) {
        cj <- .shard_codec_json_entry(cfg, cursor); bodies <- c(bodies, list(cj$body))
        centrals <- c(centrals, list(cj$central)); cursor <- cursor + length(cj$body)
    }
    cd <- do.call(c, centrals); cd_off <- cursor
    eocd <- .shard_zip_eocd(length(centrals), cd_off, length(cd))
    index <- .shard_build_index(offsets, nbytes)
    c(index, do.call(c, bodies), cd, eocd)
}
```

> **Step 5 [PIN]:** for `gzip` the index offset must point at the LFH **name
> field** (relocated gzip header), not the entry data - handled in Task 8.

- [ ] **Step 6: Regenerate roxygen collate** so `@include` ordering is right.

Run: `Rscript -e 'devtools::document()'`

- [ ] **Step 7: Run - expect PASS** (blosc env).

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-shard-zip.R")'`

- [ ] **Step 8: Commit.**

```bash
git add R/shard_zip.R R/shard_encode.R NAMESPACE tests/testthat/test-shard-zip.R
git commit -m "feat(packed-write): STORED-method ZIP dual-format framing + codec.json"
```

## Task 7: zstd method-93 framing

**Files:**
- Modify: `R/shard_zip.R`
- Test: `tests/testthat/test-shard-zip.R`

- [ ] **Step 1: Failing test** (zstd shard is a legal ZIP, method 93, round-trips):

```r
test_that("a dual-format zstd shard round-trips and is a legal ZIP", {
    skip_if_not(dafr:::dafr_have_zstd_cpp(), "libzstd not built in")
    vals <- as.numeric(1:1200)
    blob <- dafr:::.shard_assemble(vals, "float64", 1200L, 1024L, "zstd", 5L)
    node <- dafr:::.files_packed_node(
        list(eltype = "Float64", compression = "zstd", chunk_shape = list(1024L)),
        shape = 1200L, chunk_shape = 1024L)
    expect_equal(dafr:::.shard_decode_vector(blob, node), vals)
    tmp <- tempfile(fileext = ".zip"); writeBin(blob, tmp)
    z <- zip::zip_list(tmp); expect_true("c/0" %in% z$filename)
})
```

- [ ] **Step 2: Run - expect FAIL or PASS.** zstd uses method 93 with the raw zstd
frame as entry data (same offset-at-data-start as STORED) and NO `codec.json`. The
Task 6 `.shard_assemble` already handles method != 0 (no codec.json). If the test
passes once `.shard_zip_method("zstd")==93` is wired, only verify the method byte
in the headers; otherwise fix the method plumbing.

- [ ] **Step 3: Commit.**

```bash
git add R/shard_zip.R tests/testthat/test-shard-zip.R
git commit -m "feat(packed-write): zstd method-93 ZIP framing"
```

## Task 8: gzip method-8 framing (header-in-name trick) - the CRAN path

**Files:**
- Modify: `R/shard_zip.R`, `R/shard_encode.R`
- Test: `tests/testthat/test-shard-zip.R`

- [ ] **Step 1: Confirm the gzip [PIN]** against `zpk/gz.daf.zarr`. Inspect the
first inner chunk's bytes at the index offset and the LFH name field:

Run: `Rscript -e 'b<-readBin("tests/testthat/fixtures/zpk/gz.daf.zarr/vectors/cell/score/c/0","raw",n=1e6); off<-89; cat(sprintf("%02x ", b[off+1:18]))'`
Expected: a gzip stream beginning `1f 8b 08 ...` at the index offset (so the Zarr
range from the offset is a valid gzip stream). Confirm the 10-byte gzip header
sits in the LFH name field and the entry data is the raw DEFLATE payload. Record
the exact header bytes + chunk-index base64 encoding.

- [ ] **Step 2: Failing test** (gzip - runs on EVERY build, no optional lib):

```r
test_that("a dual-format gzip shard round-trips and is a legal ZIP", {
    vals <- as.numeric(1:1200)
    blob <- dafr:::.shard_assemble(vals, "float64", 1200L, 1024L, "gzip", 5L)
    node <- dafr:::.files_packed_node(
        list(eltype = "Float64", compression = "gzip", chunk_shape = list(1024L)),
        shape = 1200L, chunk_shape = 1024L)
    expect_equal(dafr:::.shard_decode_vector(blob, node), vals)
    tmp <- tempfile(fileext = ".zip"); writeBin(blob, tmp)
    z <- zip::zip_list(tmp); expect_equal(nrow(z), 2L)  # 2 DEFLATE entries, no codec.json
})
```

- [ ] **Step 3: Run - expect FAIL** (gzip currently framed as STORED-of-full-stream).

- [ ] **Step 4: Implement the gzip special case** in `R/shard_zip.R` + `.shard_assemble`:
- The LFH name field is the 10-byte gzip header `1f 8b 08 01 <b0 b1 b2 b3> 02 ff`
  where `b0..b3` = base64 of `(chunk_index-1)` in 3 bytes [PIN exact from Step 1].
- The ZIP entry data is the raw DEFLATE payload = `memCompress(plain,"gzip")` with
  the 10-byte header and 8-byte trailer stripped.
- The 8-byte gzip trailer `crc32(plain):u32 LE` + `length(plain)%%2^32:u32 LE`
  follows the entry data as dead bytes.
- The **Zarr index offset points at the LFH name-field start**, and `nbytes`
  spans `10 + length(deflate) + 8` so the Zarr range is a full gzip stream.

Add a `method == 8L` branch to the per-chunk loop in `.shard_assemble`:

```r
        if (method == 8L) {
            full <- memCompress(plain, type = "gzip")
            deflate <- full[11:(length(full) - 8L)]
            name <- .shard_gzip_name(k)               # 10-byte gzip header
            crc <- dafr_crc32_cpp(plain) %% 2^32
            trailer <- c(.shard_u32_raw(crc),
                         .shard_u32_raw(length(plain) %% 2^32))
            lfh <- .shard_zip_local_header_raw(name, crc, length(deflate),
                                               length(plain), method = 8L)
            name_off <- cursor + (length(lfh) - length(name))  # name field start
            offsets[[k]] <- name_off
            nbytes[[k]] <- length(name) + length(deflate) + length(trailer)
            centrals[[k]] <- .shard_zip_central_entry(name, crc, length(deflate),
                                                      length(plain), cursor,
                                                      method = 8L)
            bodies[[k]] <- c(lfh, deflate, trailer)
            cursor <- cursor + length(lfh) + length(deflate) + length(trailer)
            next
        }
```

- [ ] **Step 5: Run - expect PASS** (no optional lib needed). Also confirm the byte
range from `offsets[[1]]` decodes via `memDecompress(type="gzip")`.

- [ ] **Step 6: Commit.**

```bash
git add R/shard_zip.R R/shard_encode.R tests/testthat/test-shard-zip.R
git commit -m "feat(packed-write): gzip method-8 framing (header-in-name, CRAN path)"
```

---

# Phase 4 - ZarrDaf write integration

## Task 9: Packed options resolver + codec/lib validation

**Files:**
- Create: `R/packed_opts.R`
- Test: `tests/testthat/test-packed-opts.R`

- [ ] **Step 1: Failing test** `tests/testthat/test-packed-opts.R`:

```r
test_that(".packed_opts reads defaults and overrides", {
    withr::local_options(list(dafr.packed_compression = NULL,
                              dafr.packed_compression_level = NULL,
                              dafr.packed_target_chunk_kb = NULL))
    o <- dafr:::.packed_opts()
    expect_equal(o$compression, "blosc_zstd_bitshuffle")
    expect_equal(o$level, 5L); expect_equal(o$target_kb, 8L)
    withr::local_options(list(dafr.packed_compression = "gzip"))
    expect_equal(dafr:::.packed_opts()$compression, "gzip")
})

test_that(".packed_validate_codec errors when the lib is absent", {
    if (!dafr:::dafr_have_zstd_cpp())
        expect_error(dafr:::.packed_validate_codec("zstd"), "requires libzstd")
    expect_silent(dafr:::.packed_validate_codec("gzip"))
})
```

- [ ] **Step 2: Run - expect FAIL.**

- [ ] **Step 3: Implement `R/packed_opts.R`:**

```r
# R/packed_opts.R
# Resolve packed-write tuning from R options (mirrors Julia's DAF_PACKED_*
# module globals) and validate that the requested codec's optional library is
# compiled in.

.packed_opts <- function() {
    list(
        compression = getOption("dafr.packed_compression", "blosc_zstd_bitshuffle"),
        level       = as.integer(getOption("dafr.packed_compression_level", 5L)),
        target_kb   = as.integer(getOption("dafr.packed_target_chunk_kb", 8L))
    )
}

# Stop with an actionable message if `codec` needs an optional lib not built in.
.packed_validate_codec <- function(codec) {
    if (!codec %in% names(.SHARD_CODEC_TABLE)) {
        stop(sprintf("dafr packed write: unknown compression %s (supported: %s)",
                     sQuote(codec),
                     paste(names(.SHARD_CODEC_TABLE), collapse = ", ")),
             call. = FALSE)
    }
    comp <- .SHARD_CODEC_TABLE[[codec]]$compressor
    if (comp == "zstd" && !dafr_have_zstd_cpp()) {
        stop("dafr packed write: compression 'zstd' requires libzstd; install it ",
             "and reinstall dafr, or use options(dafr.packed_compression='gzip').",
             call. = FALSE)
    }
    if (comp == "blosc" && !dafr_have_blosc_cpp()) {
        stop("dafr packed write: blosc compression requires c-blosc; install it ",
             "and reinstall dafr, or use options(dafr.packed_compression='gzip').",
             call. = FALSE)
    }
    invisible(TRUE)
}
```

- [ ] **Step 4: Run - expect PASS.** Commit.

```bash
git add R/packed_opts.R tests/testthat/test-packed-opts.R
git commit -m "feat(packed-write): options resolver + codec/lib validation"
```

## Task 10: ZarrDaf sharded `zarr.json` builder

**Files:**
- Modify: `R/zarr_v3.R`
- Test: `tests/testthat/test-zarr-packed-write.R`

- [ ] **Step 1: Confirm the [PIN] sharded zarr.json** against the fixture.

Run: `Rscript -e 'cat(readLines("tests/testthat/fixtures/zpk/bz.daf.zarr/vectors/cell/score/zarr.json"))'`
Record: the `sharding_indexed` codec config (inner `chunk_shape`, inner `codecs`
incl. the exact blosc/zstd/gzip config block, `index_codecs`, `index_location`),
the outer `chunk_grid` (= shape), `chunk_key_encoding`, and the
`daf_packed_format` attribute.

- [ ] **Step 2: Failing test** in `tests/testthat/test-zarr-packed-write.R`:

```r
test_that("zarr_v3_sharded_array_meta matches the fixture structure", {
    meta <- dafr:::zarr_v3_sharded_array_meta(
        shape = 1200L, dtype = "float64", inner = 1024L,
        codec = "blosc_zstd_bitshuffle", level = 5L)
    expect_equal(meta$codecs[[1]]$name, "sharding_indexed")
    cfg <- meta$codecs[[1]]$configuration
    expect_equal(unlist(cfg$chunk_shape), 1024L)
    expect_equal(cfg$index_location, "start")
    expect_equal(meta$attributes$daf_packed_format, "indexed+zipped")
    expect_equal(unlist(meta$chunk_grid$configuration$chunk_shape), 1200L)
})
```

- [ ] **Step 3: Run - expect FAIL.**

- [ ] **Step 4: Implement `zarr_v3_sharded_array_meta()`** in `R/zarr_v3.R` (after
`zarr_v3_array_meta`). Build the inner compressor config block per codec [PIN]:

```r
# Inner-codec JSON block for a sharding_indexed codec [PIN: confirm config keys
# against the zpk fixtures in Task 10 Step 1].
.zarr_v3_inner_codec_json <- function(codec, level, typesize) {
    switch(codec,
        gzip = list(name = "gzip", configuration = list(level = level)),
        zstd = list(name = "zstd",
                    configuration = list(level = level, checksum = FALSE)),
        blosc_zstd_bitshuffle = list(name = "blosc", configuration = list(
            cname = "zstd", clevel = level, shuffle = "bitshuffle",
            typesize = typesize, blocksize = 0L)),
        blosc_lz4_bitshuffle = list(name = "blosc", configuration = list(
            cname = "lz4", clevel = level, shuffle = "bitshuffle",
            typesize = typesize, blocksize = 0L)),
        stop(sprintf("zarr_v3: unknown packed codec %s", sQuote(codec)),
             call. = FALSE))
}

# Zarr v3 array metadata for a packed (sharded) array. `shape`/`inner` are in
# on-disk order (reversed for matrices). One outer chunk == full shape.
zarr_v3_sharded_array_meta <- function(shape, dtype, inner, codec, level) {
    shape <- as.integer(shape); inner <- as.integer(inner)
    typesize <- if (dtype == "string") 1L else zarr_v3_size_for_dtype(dtype)
    array_step <- if (dtype == "string")
        list(name = "vlen-utf8",
             configuration = structure(list(), names = character(0L))) else
        list(name = "bytes", configuration = list(endian = "little"))
    fill_value <- if (dtype == "string") "" else
        if (dtype == "bool") FALSE else
        if (zarr_v3_r_kind_for_dtype(dtype) == "double") 0.0 else 0L
    list(
        zarr_format = 3L, node_type = "array",
        shape = as.list(shape), data_type = dtype,
        chunk_grid = list(name = "regular",
                          configuration = list(chunk_shape = as.list(shape))),
        chunk_key_encoding = list(name = "default",
                                  configuration = list(separator = "/")),
        codecs = list(list(name = "sharding_indexed", configuration = list(
            chunk_shape = as.list(inner),
            codecs = list(array_step,
                          .zarr_v3_inner_codec_json(codec, level, typesize)),
            index_codecs = list(list(name = "bytes",
                                     configuration = list(endian = "little")),
                                list(name = "crc32c",
                                     configuration = structure(list(),
                                         names = character(0L)))),
            index_location = "start"))),
        fill_value = fill_value,
        attributes = list(daf_packed_format = "indexed+zipped")
    )
}
```

- [ ] **Step 5: Run - expect PASS.** Commit.

```bash
git add R/zarr_v3.R tests/testthat/test-zarr-packed-write.R
git commit -m "feat(packed-write): ZarrDaf sharded array zarr.json builder"
```

## Task 11: Thread `packed=` through ZarrDaf + route dense/sparse writes

**Files:**
- Modify: `R/zarr_format.R`, `R/shard_encode.R`
- Test: `tests/testthat/test-zarr-packed-write.R`

- [ ] **Step 1: Failing end-to-end test** (write packed via `zarr_daf`, read back via the existing reader):

```r
test_that("zarr_daf(packed=TRUE) round-trips dense, matrix, sparse, strings", {
    codec <- if (dafr:::dafr_have_blosc_cpp()) "blosc_zstd_bitshuffle" else "gzip"
    withr::local_options(list(dafr.packed_compression = codec))
    dir <- withr::local_tempdir(); path <- file.path(dir, "p.daf.zarr")
    daf <- zarr_daf(path, "w", packed = TRUE)
    add_axis(daf, "cell", paste0("c", 1:1200))
    add_axis(daf, "gene", paste0("g", 1:8))
    set_vector(daf, "cell", "score", as.numeric(1:1200))             # packed
    set_vector(daf, "cell", "tag", rep("x", 1200))                   # strings
    set_matrix(daf, "cell", "gene", "dense",
               matrix(as.numeric(1:(1200*8)), 1200, 8))              # packed
    sm <- Matrix::sparseMatrix(i = sample(1200, 2000, TRUE),
                               j = ((seq_len(2000) - 1) %% 8) + 1,
                               x = as.numeric(1:2000), dims = c(1200, 8))
    set_matrix(daf, "cell", "gene", "sparse", sm)

    ro <- zarr_daf(path, "r")
    expect_equal(as.numeric(get_vector(ro, "cell", "score")), as.numeric(1:1200))
    expect_equal(as.numeric(get_matrix(ro, "cell", "gene", "dense")),
                 as.numeric(1:(1200*8)))
    expect_equal(unname(get_vector(ro, "cell", "tag"))[1], "x")
    expect_equal(Matrix::nnzero(get_matrix(ro, "cell", "gene", "sparse")),
                 Matrix::nnzero(sm))
    # The score array must actually be sharded on disk.
    node <- dafr:::zarr_v3_read_array(dafr:::S7_prop_store(ro), "vectors/cell/score")
    expect_true(dafr:::.zarr_is_sharded(node))
})
```

> If a `S7_prop_store` test helper does not exist, read the node via
> `dafr:::zarr_v3_read_array(S7::prop(ro, "store"), "vectors/cell/score")`.

- [ ] **Step 2: Run - expect FAIL** (`packed` arg unknown / writes flat).

- [ ] **Step 3: Add a sharded-or-flat component writer** to `R/shard_encode.R`:

```r
# Write one array component to a zarr store, sharded if `packed` and over
# threshold, else flat. `shape`/`values` are in on-disk order (caller reverses
# matrices). dtype is the v3 dtype string.
.shard_write_zarr_component <- function(store, base, values, shape, dtype, packed) {
    opts <- .packed_opts()
    if (packed && .shard_should_pack(shape[[1L]], dtype, opts$target_kb)) {
        .packed_validate_codec(opts$compression)
        inner <- .shard_inner_chunk_shape(shape, dtype, opts$target_kb)
        zarr_v3_write_array(store, base, zarr_v3_sharded_array_meta(
            shape = shape, dtype = dtype, inner = inner,
            codec = opts$compression, level = opts$level))
        blob <- .shard_assemble(values, dtype, shape, inner,
                                opts$compression, opts$level)
        store_set_bytes(store, zarr_v3_chunk_path(base, length(shape)), blob)
    } else {
        zarr_v3_write_array(store, base, zarr_v3_array_meta(shape = shape,
                                                            dtype = dtype))
        chunk <- if (dtype == "string") zarr_v3_encode_strings(values) else
            zarr_v3_encode_chunk(values, dtype)
        store_set_bytes(store, zarr_v3_chunk_path(base, length(shape)), chunk)
    }
    invisible()
}
```

- [ ] **Step 4: Thread `packed` into `zarr_daf()`** (`R/zarr_format.R:73`): add
`packed = FALSE` to the signature and `internal$packed <- isTRUE(packed)` next to
`internal$mode <- mode` (L155). Add an accessor:

```r
.zarr_is_packed <- function(daf) isTRUE(S7::prop(daf, "internal")$packed)
```

- [ ] **Step 5: Route dense writes.** Change `.zarr_write_dense_array` (L314) to
take the daf's packed flag. The simplest non-invasive change: give it a `packed`
arg defaulting FALSE and pass `.zarr_is_packed(daf)` from each caller
(`.zarr_write_scalar`, axis entries, `.zarr_write_dense_vector`,
`.zarr_write_dense_matrix`). Body becomes a thin wrapper over
`.shard_write_zarr_component`:

```r
.zarr_write_dense_array <- function(store, base, values, shape, packed = FALSE) {
    dtype <- zarr_v3_dtype_for_r(values)
    .shard_write_zarr_component(store, base, values, as.integer(shape), dtype,
                                packed)
    invisible()
}
```

> Scalars and axes: keep `packed = FALSE` (scalars are length-1; axis entry
> vectors are strings - if an axis is huge it can still pack, but Julia leaves
> axes flat; pass FALSE to match). Dense vector/matrix callers pass the daf flag.

- [ ] **Step 6: Route sparse component writes.** In `.zarr_write_sparse_vector`
(L744) and `.zarr_write_sparse_matrix` (L1054), replace each
`zarr_v3_write_array(...) + store_set_bytes(zarr_v3_chunk_path(...),
zarr_v3_encode_chunk(...))` component pair with
`.shard_write_zarr_component(store, <comp_base>, <values>, length(<values>),
<dtype>, packed)` (each component is 1-D, thresholded independently). Thread
`packed` into both functions and their callers (the `format_set_vector` /
`format_set_matrix` methods at L685 / L994 pass `.zarr_is_packed(daf)`).

- [ ] **Step 7: Run - expect PASS.**

Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-zarr-packed-write.R")'`

- [ ] **Step 8: Commit.**

```bash
git add R/zarr_format.R R/shard_encode.R tests/testthat/test-zarr-packed-write.R
git commit -m "feat(packed-write): ZarrDaf packed write integration (dense + sparse + strings)"
```

## Task 12: ZarrDaf interop - Julia reads dafr-written packed store

**Files:**
- Test: `tests/testthat/test-zarr-packed-write-interop.R`

- [ ] **Step 1: Test** (skips when the Julia env / lib is absent; reuse the read
port's `run_julia` / `.daf_jl_uses_zarr_v3` helpers if present, else `skip`):

```r
test_that("DataAxesFormats.jl reads a dafr-written packed ZarrDaf store", {
    skip_on_cran()
    skip_if_not(exists(".daf_jl_uses_zarr_v3") && .daf_jl_uses_zarr_v3())
    skip_if_not(dafr:::dafr_have_blosc_cpp(), "c-blosc not built in")
    dir <- withr::local_tempdir(); path <- file.path(dir, "p.daf.zarr")
    withr::local_options(list(dafr.packed_compression = "blosc_zstd_bitshuffle"))
    daf <- zarr_daf(path, "w", packed = TRUE)
    add_axis(daf, "cell", paste0("c", 1:1500))
    set_vector(daf, "cell", "v", as.numeric(1:1500))
    res <- run_julia(c(
        "using DataAxesFormats",
        sprintf('d = ZarrDaf(raw"%s", "r")', path),
        'v = get_vector(d, "cell", "v")',
        'println(v[1] == 1.0 && v[1500] == 1500.0 ? "OK" : "BAD")'))
    expect_true(any(grepl("^OK$", res)))
})
```

- [ ] **Step 2: Run - expect PASS** (conda env). If Julia errors on the shard,
diff dafr's blob framing against the matching `zpk` fixture region and fix.

- [ ] **Step 3: Commit.**

```bash
git add tests/testthat/test-zarr-packed-write-interop.R
git commit -m "test(packed-write): Julia reads dafr-written packed ZarrDaf store"
```

---

# Phase 5 - FilesDaf/ZipDaf write integration

## Task 13: Packed FilesDaf descriptor fields

**Files:**
- Modify: `R/files_io.R`
- Test: `tests/testthat/test-files-packed-write.R`

- [ ] **Step 1: Confirm the [PIN] descriptor** against the FilesDaf fixture
generator output. Regenerate a packed FilesDaf fixture if none is committed:

Run: `conda run -n dafr-mcview julia dev/scripts/gen_files_packed_fixtures.jl /tmp/fp && cat /tmp/fp/*/vectors/*/score.json 2>/dev/null | head`
Record the exact packed descriptor keys (`format`, `eltype`, `n_elements`,
`packed_format`, `chunk_shape`, `compression`, `compression_level`,
`index_location`).

- [ ] **Step 2: Failing test** in `tests/testthat/test-files-packed-write.R`:

```r
test_that(".files_packed_descriptor emits the expected fields", {
    d <- dafr:::.files_packed_descriptor("Float64", n = 1200L, inner = 1024L,
                                         codec = "gzip", level = 5L)
    expect_equal(d$format, "dense")
    expect_equal(d$eltype, "Float64")
    expect_equal(d$packed_format, "indexed+zipped")
    expect_equal(unlist(d$chunk_shape), 1024L)
    expect_equal(d$compression, "gzip")
    expect_equal(d$index_location, "start")
})
```

- [ ] **Step 3: Implement** in `R/files_io.R` a descriptor builder used by both the
dense and per-component writers:

```r
# A packed dense-component descriptor (the `.json` body for a `<name>.zip` shard).
.files_packed_descriptor <- function(eltype, n, inner, codec, level) {
    list(format = "dense", eltype = eltype, n_elements = as.integer(n),
         packed_format = "indexed+zipped",
         chunk_shape = as.list(as.integer(inner)),
         compression = codec, compression_level = as.integer(level),
         index_location = "start")
}
```

- [ ] **Step 4: Run - expect PASS.** Commit.

```bash
git add R/files_io.R tests/testthat/test-files-packed-write.R
git commit -m "feat(packed-write): FilesDaf packed descriptor builder"
```

## Task 14: Thread `packed=` through FilesDaf + write `.zip` shards

**Files:**
- Modify: `R/files_daf.R`, `R/files_daf_write.R`, `R/files_io.R`
- Test: `tests/testthat/test-files-packed-write.R`

- [ ] **Step 1: Failing end-to-end test** (write packed FilesDaf, read back via the existing reader):

```r
test_that("files_daf(packed=TRUE) round-trips dense, matrix, sparse", {
    codec <- if (dafr:::dafr_have_blosc_cpp()) "blosc_zstd_bitshuffle" else "gzip"
    withr::local_options(list(dafr.packed_compression = codec))
    dir <- withr::local_tempdir(); path <- file.path(dir, "p.files-daf")
    daf <- files_daf(path, "w", packed = TRUE)
    add_axis(daf, "cell", paste0("c", 1:1200))
    add_axis(daf, "gene", paste0("g", 1:8))
    set_vector(daf, "cell", "score", as.numeric(1:1200))
    set_matrix(daf, "cell", "gene", "dense",
               matrix(as.numeric(1:(1200*8)), 1200, 8))
    ro <- files_daf(path, "r")
    expect_equal(as.numeric(get_vector(ro, "cell", "score")), as.numeric(1:1200))
    expect_equal(as.numeric(get_matrix(ro, "cell", "gene", "dense")),
                 as.numeric(1:(1200*8)))
    # score must be a .zip shard on disk.
    expect_true(file.exists(file.path(path, "vectors", "cell", "score.zip")))
})
```

- [ ] **Step 2: Run - expect FAIL.**

- [ ] **Step 3: Add a sharded-or-flat Files component writer** to `R/files_io.R`
(writes either `<path>.data`/component file + flat descriptor, or `<path>.zip` +
packed descriptor). It returns the descriptor so the caller assembles the `.json`:

```r
# Write one 1-D component as a packed `<base>.zip` shard (+ return packed
# descriptor) when packed & over threshold, else flat `<base><ext>` (+ flat
# descriptor). `values` already in element order; `dtype` the v3 dtype; `eltype`
# the Julia name for the descriptor.
.files_write_component <- function(base, ext, values, dtype, eltype, packed) {
    opts <- .packed_opts()
    n <- length(values)
    if (packed && .shard_should_pack(n, dtype, opts$target_kb)) {
        .packed_validate_codec(opts$compression)
        inner <- .shard_inner_chunk_shape(n, dtype, opts$target_kb)
        blob <- .shard_assemble(values, dtype, n, inner, opts$compression,
                                opts$level)
        writeBin(blob, paste0(base, ".zip"))
        return(.files_packed_descriptor(eltype, n, inner, opts$compression,
                                        opts$level))
    }
    .write_bin_dense(paste0(base, ext), values, dtype)
    list(format = "dense", eltype = eltype, n_elements = as.integer(n))
}
```

- [ ] **Step 4: Thread `packed` into `files_daf()`** (`R/files_daf.R:34`): add
`packed = FALSE`, store `internal$packed <- isTRUE(packed)`; add
`.files_is_packed_writer <- function(daf) isTRUE(S7::prop(daf, "internal")$packed)`.

- [ ] **Step 5: Route the write sites** in `R/files_daf_write.R`:
- `.files_write_vector_dense` (L136): for the numeric branch, use
  `.files_write_component(file.path(vdir, name), ".data", vec,
  zarr_v3_dtype_for_r(vec), .files_eltype_name(vec), packed)` and write the
  returned descriptor via `.write_descriptor` (replacing
  `.write_descriptor_dense`). Strings stay flat (`<name>.txt`) - Julia packs
  strings too, but defer string packing to a follow-up; pass FALSE for the string
  branch and note it.
- `.files_write_vector_sparse_numeric` (L159) and `.files_write_matrix_sparse`
  (L298): build each component (`nzind`/`nzval`/`colptr`/`rowval`) via
  `.files_write_component(file.path(<dir>, paste0(name, ".", comp)), paste0(".",
  comp), <vals>, <dtype>, <eltype>, packed)`, collect the returned per-component
  descriptors, and write the sparse `.json` via `.write_descriptor_sparse`.
- dense matrix set (L344-375): numeric branch uses `.files_write_component` with
  `as.vector(mat)` (column-major) and on-disk handling consistent with the reader
  (`.files_packed_decode_matrix` expects natural `[nrows, ncols]` chunk_shape) -
  so pass `chunk_shape` as `[n_chunk_rows, 1]` and `n = nrow*ncol`... **[PIN]**:
  the matrix packed path needs the matrix-shaped assemble, not the 1-D one. Use
  `.shard_assemble(as.vector(mat), dtype, shape=c(nr,nc), inner=c(n_chunk_rows,1),
  ...)` and a matrix descriptor `chunk_shape=[n_chunk_rows,1]`. Confirm against the
  FilesDaf matrix fixture before finalizing.

Thread `packed = .files_is_packed_writer(daf)` from the `format_set_vector` /
`format_set_matrix` methods (L212 / L344) into these helpers.

- [ ] **Step 6: Run - expect PASS.**

- [ ] **Step 7: Commit.**

```bash
git add R/files_daf.R R/files_daf_write.R R/files_io.R tests/testthat/test-files-packed-write.R
git commit -m "feat(packed-write): FilesDaf packed write integration (.zip shards)"
```

## Task 15: FilesDaf interop - Julia reads dafr-written packed FilesDaf store

**Files:**
- Test: `tests/testthat/test-files-packed-write-interop.R`

- [ ] **Step 1: Test** (mirrors Task 12 for `FilesDaf`):

```r
test_that("DataAxesFormats.jl reads a dafr-written packed FilesDaf store", {
    skip_on_cran()
    skip_if_not(exists("run_julia"))
    skip_if_not(dafr:::dafr_have_blosc_cpp(), "c-blosc not built in")
    dir <- withr::local_tempdir(); path <- file.path(dir, "p.files-daf")
    withr::local_options(list(dafr.packed_compression = "blosc_zstd_bitshuffle"))
    daf <- files_daf(path, "w", packed = TRUE)
    add_axis(daf, "cell", paste0("c", 1:1500))
    set_vector(daf, "cell", "v", as.numeric(1:1500))
    res <- run_julia(c(
        "using DataAxesFormats",
        sprintf('d = FilesDaf(raw"%s", "r")', path),
        'v = get_vector(d, "cell", "v")',
        'println(v[1] == 1.0 && v[1500] == 1500.0 ? "OK" : "BAD")'))
    expect_true(any(grepl("^OK$", res)))
})
```

- [ ] **Step 2: Run - expect PASS** (conda env). Commit.

```bash
git add tests/testthat/test-files-packed-write-interop.R
git commit -m "test(packed-write): Julia reads dafr-written packed FilesDaf store"
```

---

# Phase 6 - Docs, errors, CRAN verification

## Task 16: Codec-absent error coverage

**Files:**
- Test: `tests/testthat/test-zarr-packed-write.R` (append)

- [ ] **Step 1: Test** that requesting a missing-lib codec errors actionably:

```r
test_that("packed write errors actionably when the codec lib is absent", {
    if (dafr:::dafr_have_zstd_cpp()) skip("libzstd present")
    withr::local_options(list(dafr.packed_compression = "zstd"))
    dir <- withr::local_tempdir(); path <- file.path(dir, "p.daf.zarr")
    daf <- zarr_daf(path, "w", packed = TRUE)
    add_axis(daf, "cell", paste0("c", 1:1200))
    expect_error(set_vector(daf, "cell", "score", as.numeric(1:1200)),
                 "requires libzstd")
})
```

- [ ] **Step 2: Run - expect PASS or SKIP.** Commit.

```bash
git add tests/testthat/test-zarr-packed-write.R
git commit -m "test(packed-write): actionable error when codec lib absent"
```

## Task 17: Docs, NEWS, DESCRIPTION, REMAINING-GAP, full verification

**Files:**
- Modify: `R/zarr_format.R`, `R/files_daf.R` (roxygen `packed=`), `DESCRIPTION`, `NEWS.md`, `dev/parity-audit-2026-06-11/REMAINING-GAP.md`

- [ ] **Step 1: Document `packed=`** on both constructors' roxygen (`@param packed`
explaining opt-in + the three `options(dafr.packed_*)` knobs and that
blosc/zstd need the optional libs; gzip is always available).

- [ ] **Step 2: `DESCRIPTION`** - extend `SystemRequirements` to mention the
optional libs are also used for *writing* packed stores (read wording already
present).

- [ ] **Step 3: `NEWS.md`** - new subsection:

```markdown
## ZarrDaf + FilesDaf: packed/sharded WRITE

* Writing packed/sharded (`packed = TRUE`) ZarrDaf and FilesDaf/ZipDaf stores is
  now supported, producing the same dual-format ("indexed+zipped") shards
  DataAxesFormats.jl writes. Opt in with `zarr_daf(..., packed = TRUE)` /
  `files_daf(..., packed = TRUE)`; tune via `options(dafr.packed_compression=,
  dafr.packed_compression_level=, dafr.packed_target_chunk_kb=)`
  (default `blosc_zstd_bitshuffle`, level 5, 8 KiB).
* `gzip` packed write needs no extra library; `zstd` / `blosc_*` need the same
  optional system libzstd / c-blosc probed by `configure` for packed reads.
```

- [ ] **Step 4: `REMAINING-GAP.md`** - move "packed/sharded WRITE" out of DEFERRED
BACKENDS (mark done in working tree).

- [ ] **Step 5: Document regen + full suite.**

Run: `Rscript -e 'devtools::document()'`
Run: `NOT_CRAN=true Rscript -e 'pkgload::load_all("."); testthat::test_dir("tests/testthat", reporter="summary", stop_on_failure=FALSE)'`
Expected: 0 failures (blosc/zstd live tests run in the env; no-lib tests skip).

- [ ] **Step 6: CRAN no-lib check.** Rebuild with empty configure flags and confirm
the flat path + gzip packed path pass and blosc/zstd packed tests skip/err:

Run: `DAFR_SHARD_CPPFLAGS= DAFR_SHARD_LIBS= R CMD INSTALL --preclean . && NOT_CRAN=true Rscript -e 'library(dafr); testthat::test_dir("tests/testthat", filter="packed", reporter="summary", stop_on_failure=FALSE)'`
(Or sed the Makevars subs empty per the read port's Task 13.) Expected: gzip
packed read+write pass; blosc/zstd packed tests skip; flat tests pass.

- [ ] **Step 7: rcmdcheck (ship gate per project memory).**

Run: `Rscript -e 'rcmdcheck::rcmdcheck(args = c("--as-cran","--no-manual"), error_on = "warning")'`
Expected: 0 errors / 0 warnings.

- [ ] **Step 8: Commit.**

```bash
git add R man DESCRIPTION NEWS.md dev/parity-audit-2026-06-11/REMAINING-GAP.md
git commit -m "docs(packed-write): constructor packed=, NEWS, DESCRIPTION, gap update"
```

---

## Self-review notes

- **Spec coverage:** §3 module layout -> Tasks 1-3,6,9,10; §4 codecs/CRAN gate ->
  Tasks 2,4,8,16; §5 index -> Task 5; §6 threshold/grid -> Tasks 3,5; §7 ZIP
  framing -> Tasks 6-8; §8 parity caveat -> verification via read-core round-trip
  (every encode task) + interop (Tasks 12,15); §9 ZarrDaf meta -> Task 10; §10
  FilesDaf descriptor -> Task 13; §11 verification -> the test file per task; §12
  phasing -> the six phases.
- **[PIN] items** (confirm against fixtures before trusting): threshold/inner shape
  (Task 3 Step 1), STORED framing + EOCD (Task 6 Step 1), gzip header-in-name
  (Task 8 Step 1), sharded zarr.json inner-codec config (Task 10 Step 1), FilesDaf
  descriptor + matrix chunk_shape (Tasks 13 Step 1, 14 Step 5).
- **Type consistency:** `.SHARD_CODEC_TABLE` defined in Task 4 is reused in Tasks
  5,9,11; `.shard_assemble(values, dtype, shape, inner, codec, level, cname)`
  signature stable from Task 6; `.shard_write_zarr_component` /
  `.files_write_component` are the two integration chokepoints.
- **Deferred (logged, not silently dropped):** FilesDaf **string** component packing
  is left flat in Task 14 Step 5 (Julia packs strings; a follow-up can route the
  string branch through `.files_write_component` with dtype "string"). ZipDaf write
  rides the FilesDaf path (shards written into the zip store); if ZipDaf lacks a
  flat writer entirely, that is out of scope here and must be flagged at exit.
```
