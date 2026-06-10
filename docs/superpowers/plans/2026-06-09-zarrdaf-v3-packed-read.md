# ZarrDaf v3 packed/sharded READ Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Read packed/sharded (`packed=true`) Zarr v3 ZarrDaf stores in dafr (never write them), keeping CRAN green when no compression library is present.

**Architecture:** A packed property's chunk blob (`c/0` 1-D, `c/0/0` 2-D) is a *dual-format shard*: a ZIP archive that is simultaneously a Zarr v3 ZEP-0002 sharded array. We read it purely as a Zarr sharded array - parse the start-located shard index (`(offset:u64, nbytes:u64)` pairs + a trailing crc32c), then for each inner chunk run the reverse inner-codec pipeline and tile the chunks into the full array in Julia column-major order. The ZIP framing is ignored. `crc32c` (Castagnoli) is a tiny always-compiled C table. The compression backends are `configure`-gated optional system libraries: `c-blosc` (the default `blosc_zstd_bitshuffle` / `blosc_lz4_bitshuffle` codecs) and `libzstd` (the plain `zstd` codec); `gzip` decodes with base-R `memDecompress` (no dependency). Without the optional libs, the flat path is untouched and decoding a blosc/zstd shard raises an actionable error.

**Tech Stack:** R (S7, jsonlite, bit64, Matrix), cpp11/C (crc32c table; `#ifdef`-gated blosc + zstd wrappers), autoconf-style `configure` + `configure.win`, testthat. Fixtures generated from DataAxesFormats.jl 0.3.0 via `conda run -n dafr-mcview julia`.

---

## Verified ground-truth (from real DAF 0.3.0 `packed=true` fixtures, 2026-06-09)

Pin these against `/tmp/daf-packed-fixtures/*.daf.zarr` (regenerate via `dev/scripts/gen_packed_fixtures.jl`, Task 1). **Do not re-derive; these are observed bytes.**

1. **Packing is per-component AND threshold-gated, even under `packed=true`.** A component is sharded only if its uncompressed size exceeds `DAF_PACKED_TARGET_CHUNK_KB = 8` KiB. Float64 ⇒ > 1024 elems. Sub-threshold components (most sparse `colptr`/`rowval`/`nzval`, short vectors, all string axes) stay **flat** and the existing reader handles them. A sharded sparse component is just a sharded 1-D array.

2. **Detection:** an array node is sharded iff `node$codecs[[1]]$name == "sharding_indexed"`. Secondary hint: `node$attributes$daf_packed_format == "indexed+zipped"`. The sharding config lives at `node$codecs[[1]]$configuration`:
   - `chunk_shape` — the **inner** chunk shape (e.g. `[1024]` for a vector, `[1, 1024]` for a matrix).
   - `codecs` — inner pipeline: `[{name:"bytes",configuration:{endian:"little"}}, {name:"blosc"|"zstd"|"gzip", ...}]`.
   - `index_codecs` — `[{name:"bytes",configuration:{endian:"little"}}, {name:"crc32c"}]`.
   - `index_location` — `"start"`.
   The **outer** chunk grid (`node$chunk_grid.configuration.chunk_shape`) equals the full `shape` (one outer chunk), so the shard blob is at the same single-chunk key the flat path uses: `zarr_v3_chunk_path(base, ndim)` = `c/0` (1-D) / `c/0/0` (2-D).

3. **Shard index (at start):** `N` inner chunks ⇒ `N * 16` bytes of `(offset:u64 LE, nbytes:u64 LE)` pairs, then a **4-byte crc32c** (Castagnoli) over those `N*16` bytes. `N = prod_d ceil(outer_dim[d] / inner_dim[d])`, with dims in the array's on-disk order. Inner-chunk linear index is **row-major (C-order)** over the inner-chunk grid. Empty/all-fill chunk sentinel: `offset == nbytes == 0xFFFFFFFFFFFFFFFF`.
   - *Observed (score vector, shape `[1200]`, inner `[1024]`, N=2):* bytes `59 00.. 00 | 0e 01.. 00 | 9c 01.. 00 | 76 00.. 00 | 08 d5 e9 5d`. ⇒ entry0 `(offset=89, nbytes=270)`, entry1 `(offset=412, nbytes=118)`, crc32c `0x5de9d508`. Index size = 2*16 + 4 = 36 bytes; first inner chunk begins at offset 89.

4. **Inner chunk bytes** at `(offset, nbytes)` are the inner-codec output (the `bytes` endian step is a no-op on little-endian). Decode per the Zarr inner codec, **NOT** the ZIP method:
   - **blosc** (`blosc_zstd_bitshuffle` default, `blosc_lz4_bitshuffle`): a classic **blosc1** chunk (16-byte header: `version, versionlz, flags, typesize, nbytes:u32, blocksize:u32, cbytes:u32`). bit/byte-shuffle + zstd/lz4 are internal to the chunk. One `blosc_decompress` call yields the raw little-endian element bytes. *Observed score inner-0 at offset 89:* `02 01 94 01 | 00 20 00 00 | 00 20 00 00 | 0e 01 00 00` ⇒ nbytes(uncompressed)=8192=1024*8, cbytes=270 = index nbytes. Needs **c-blosc**.
   - **zstd** (plain): a raw zstd frame (magic `28 b5 2f fd`) at the indexed offset. Needs **libzstd** (`ZSTD_decompress`).
   - **gzip**: a full gzip stream (magic `1f 8b 08`) at the indexed offset. Decode with base-R `memDecompress(x, type="gzip")`. **No dependency.**

5. **Partial last inner chunk** is fill-padded to the full inner-chunk shape before compression, so every inner chunk decompresses to `prod(inner_chunk_shape)` elements; slice the valid region during reassembly.

6. **Byte order / reassembly.** The on-disk array order matches the flat path: the array's flat element stream is Julia column-major over the Daf dims. For a matrix the on-disk `shape` is reversed `[n_cols, n_rows]`; the flat reader sets `nr = shape[[2]]`, `nc = shape[[1]]` and reads column-major into `dim=c(nr,nc)`. For the sharded case, reassemble the inner chunks into a flat buffer in **on-disk C-order** (the buffer the flat path would have mmapped), then hand that buffer to the *same* decode/dim logic. Concretely: decompress inner chunk at grid coord `g` (a vector of per-dim chunk indices) to `prod(inner_shape)` elements in C-order; for each valid local coordinate, scatter to the global flat index computed with on-disk C-order strides. Pin the exact mapping with the dense-matrix fixture round-trip in Task 11 before building sparse on top.

---

## File Structure

- **Create** `src/crc32c.c` + `src/crc32c.h` — Castagnoli CRC-32C, table-based, always compiled. Pure C, no deps.
- **Create** `src/shard_codecs.cpp` — cpp11 wrappers: `crc32c_raw()` (always), `blosc_decompress_raw()` / `zstd_decompress_raw()` (`#ifdef HAVE_BLOSC` / `HAVE_ZSTD`; else error stubs).
- **Create** `configure` + `configure.win` — probe `blosc.h`/`-lblosc` and `zstd.h`/`-lzstd`; emit `src/Makevars` from `src/Makevars.in` with `-DHAVE_BLOSC`/`-DHAVE_ZSTD` + libs.
- **Create** `src/Makevars.in` — template; **rename** current `src/Makevars` content into it (configure generates `src/Makevars`).
- **Create** `R/zarr_sharded.R` — pure-R shard parse + inner-codec dispatch + reassembly; the only consumer of the C wrappers.
- **Create** `dev/scripts/gen_packed_fixtures.jl` — reproducible fixture generator.
- **Create** `tests/testthat/fixtures/daf030-packed/*.daf.zarr` — committed small packed fixtures (one per inner codec; dense vector + dense matrix + a sharded sparse matrix).
- **Create** tests: `tests/testthat/test-crc32c.R`, `test-zarr-sharded.R`, `test-zarr-packed-read.R`, `test-zarr-packed-interop.R`.
- **Modify** `R/zarr_format.R` — route sharded arrays to `zarr_sharded.R` at the dense-vector, dense-matrix, and sparse-component read sites; gate `.zarr_try_mmap_dense` off when sharded.
- **Modify** `R/cpp11.R` + `src/cpp11.cpp` (regenerated by `cpp11::cpp_register`) — register the new entry points.
- **Modify** `DESCRIPTION` — `SystemRequirements: C++17; optional libblosc, libzstd (packed ZarrDaf read)`.
- **Modify** `environment.yml` — uncomment `c-blosc`; add `zstd`.
- **Modify** `NEWS.md` — packed-read entry; flip the "Known limitations" packed bullet.
- **Modify** `.Rbuildignore` — ignore `dev/scripts`, keep `configure*`.

---

## Task 1: Reproducible fixture generator + committed fixtures

**Files:**
- Create: `dev/scripts/gen_packed_fixtures.jl`
- Create: `tests/testthat/fixtures/daf030-packed/` (generated, committed)
- Test: (manual generation step; consumed by later tasks)

- [ ] **Step 1: Write the generator.** `dev/scripts/gen_packed_fixtures.jl`:

```julia
# Regenerate committed packed-read fixtures. Run:
#   conda run -n dafr-mcview julia dev/scripts/gen_packed_fixtures.jl <out_dir>
using DataAxesFormats
using SparseArrays
import DataAxesFormats.PackedFormat as PF

out = length(ARGS) >= 1 ? ARGS[1] : "tests/testthat/fixtures/daf030-packed"
rm(out; force=true, recursive=true); mkpath(out)

# Keep fixtures SMALL but above the 8 KiB pack threshold so dense vector,
# dense matrix, and the sparse matrix's nzval/rowval all sustain sharding.
ncell, ngene = 1200, 8
cells = ["c$(i)" for i in 1:ncell]; genes = ["g$(j)" for j in 1:ngene]

function populate!(d)
    add_axis!(d, "cell", cells); add_axis!(d, "gene", genes)
    set_scalar!(d, "name", "packed!")
    set_vector!(d, "cell", "score", Float64.(1:ncell))          # 9600 B -> sharded
    set_vector!(d, "cell", "label", ["v$(i)" for i in 1:ncell]) # strings -> flat
    set_matrix!(d, "cell", "gene", "dense",
                reshape(Float64.(1:(ncell*ngene)), ncell, ngene)) # sharded
    # Dense-enough sparse: ~2000 nonzeros so nzval (16 KB) shards.
    I = repeat(1:ncell, outer=2)[1:2000]
    J = [((k-1) % ngene)+1 for k in 1:2000]
    V = Float64.(1:2000)
    set_matrix!(d, "cell", "gene", "sparse", sparse(I, J, V, ncell, ngene))
end

for (codec, label) in [(:blosc_zstd_bitshuffle,"blosc_zstd_bitshuffle"),
                       (:blosc_lz4_bitshuffle,"blosc_lz4_bitshuffle"),
                       (:zstd,"zstd"), (:gzip,"gzip")]
    PF.DAF_PACKED_COMPRESSION = codec
    d = ZarrDaf(joinpath(out, "$(label).daf.zarr"), "w"; name=label, packed=true)
    populate!(d)
    println("WROTE $(label)")
end
```

- [ ] **Step 2: Generate into the fixtures dir.**

Run: `cd /home/aviezerl/src/dafr-native && conda run -n dafr-mcview julia dev/scripts/gen_packed_fixtures.jl tests/testthat/fixtures/daf030-packed`
Expected: four `WROTE <codec>` lines; `tests/testthat/fixtures/daf030-packed/{blosc_zstd_bitshuffle,blosc_lz4_bitshuffle,zstd,gzip}.daf.zarr/` exist.

- [ ] **Step 3: Sanity-check shard presence.**

Run: `python3 -c "import json;d=json.load(open('tests/testthat/fixtures/daf030-packed/gzip.daf.zarr/vectors/cell/score/zarr.json'));print(d['codecs'][0]['name'])"`
Expected: `sharding_indexed`

- [ ] **Step 4: Commit.**

```bash
git add dev/scripts/gen_packed_fixtures.jl tests/testthat/fixtures/daf030-packed
git commit -m "test(zarr-packed): committed DAF 0.3.0 packed fixtures + generator"
```

---

## Task 2: crc32c (Castagnoli) C core, always compiled

**Files:**
- Create: `src/crc32c.h`, `src/crc32c.c`
- Test: (exercised through R in Task 4)

- [ ] **Step 1: Header `src/crc32c.h`.**

```c
#ifndef DAFR_CRC32C_H
#define DAFR_CRC32C_H
#include <stddef.h>
#include <stdint.h>
/* Castagnoli CRC-32C (poly 0x1EDC6F41, reflected 0x82F63B78), zlib-style:
   seed 0, returns the finalized checksum for buf[0..len). */
uint32_t dafr_crc32c(const unsigned char *buf, size_t len);
#endif
```

- [ ] **Step 2: Implementation `src/crc32c.c`** (reflected, table-on-first-use; matches Zarr/zstd crc32c which finalizes with `~crc`).

```c
#include "crc32c.h"
static uint32_t TBL[256];
static int READY = 0;
static void init_table(void) {
    for (uint32_t n = 0; n < 256; n++) {
        uint32_t c = n;
        for (int k = 0; k < 8; k++)
            c = (c & 1) ? (0x82F63B78u ^ (c >> 1)) : (c >> 1);
        TBL[n] = c;
    }
    READY = 1;
}
uint32_t dafr_crc32c(const unsigned char *buf, size_t len) {
    if (!READY) init_table();
    uint32_t crc = 0xFFFFFFFFu;
    for (size_t i = 0; i < len; i++)
        crc = TBL[(crc ^ buf[i]) & 0xFF] ^ (crc >> 8);
    return crc ^ 0xFFFFFFFFu;
}
```

- [ ] **Step 3: Commit.**

```bash
git add src/crc32c.h src/crc32c.c
git commit -m "feat(zarr-packed): add always-compiled crc32c (Castagnoli) C core"
```

---

## Task 3: cpp11 wrappers (crc32c always; blosc/zstd gated)

**Files:**
- Create: `src/shard_codecs.cpp`
- Modify: `R/cpp11.R`, `src/cpp11.cpp` (regenerated)

- [ ] **Step 1: Write `src/shard_codecs.cpp`.**

```cpp
#include <cpp11.hpp>
#include <cstring>
#include <vector>
#include "crc32c.h"
#ifdef HAVE_BLOSC
#include <blosc.h>
#endif
#ifdef HAVE_ZSTD
#include <zstd.h>
#endif

using namespace cpp11;

[[cpp11::register]]
double dafr_crc32c_cpp(raws x) {
    const unsigned char* p = RAW(as_sexp(x));
    return (double)dafr_crc32c(p, x.size());
}

// Decompress a classic blosc1 chunk. out_nbytes = the chunk's uncompressed
// size (the caller reads it from the blosc header / inner-chunk element count).
[[cpp11::register]]
raws dafr_blosc_decompress_cpp(raws src, double out_nbytes) {
#ifdef HAVE_BLOSC
    size_t n = (size_t)out_nbytes;
    writable::raws out(n);
    const void* s = (const void*)RAW(as_sexp(src));
    void* d = (void*)RAW(as_sexp(out));
    int got = blosc_decompress(s, d, n);
    if (got < 0) cpp11::stop("blosc_decompress failed (code %d)", got);
    if ((size_t)got != n)
        cpp11::stop("blosc_decompress size mismatch: got %d want %zu", got, n);
    return out;
#else
    cpp11::stop("Reading blosc-packed ZarrDaf requires c-blosc; install it "
                "(e.g. `conda install c-blosc`) and reinstall dafr.");
#endif
}

[[cpp11::register]]
raws dafr_zstd_decompress_cpp(raws src, double out_nbytes) {
#ifdef HAVE_ZSTD
    size_t n = (size_t)out_nbytes;
    writable::raws out(n);
    const void* s = (const void*)RAW(as_sexp(src));
    void* d = (void*)RAW(as_sexp(out));
    size_t got = ZSTD_decompress(d, n, s, src.size());
    if (ZSTD_isError(got)) cpp11::stop("ZSTD_decompress failed: %s",
                                       ZSTD_getErrorName(got));
    if (got != n) cpp11::stop("ZSTD_decompress size mismatch");
    return out;
#else
    cpp11::stop("Reading zstd-packed ZarrDaf requires libzstd; install it "
                "(e.g. `conda install zstd`) and reinstall dafr.");
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
```

- [ ] **Step 2: Regenerate cpp11 bindings.**

Run: `cd /home/aviezerl/src/dafr-native && Rscript -e 'cpp11::cpp_register()'`
Expected: `R/cpp11.R` and `src/cpp11.cpp` gain `dafr_crc32c_cpp`, `dafr_blosc_decompress_cpp`, `dafr_zstd_decompress_cpp`, `dafr_have_blosc_cpp`, `dafr_have_zstd_cpp`.

- [ ] **Step 3: Commit.**

```bash
git add src/shard_codecs.cpp R/cpp11.R src/cpp11.cpp
git commit -m "feat(zarr-packed): cpp11 wrappers for crc32c + gated blosc/zstd decode"
```

---

## Task 4: configure / configure.win / Makevars.in

**Files:**
- Create: `src/Makevars.in`, `configure`, `configure.win`
- Modify: `.Rbuildignore`
- Remove from VCS-as-generated: `src/Makevars` (becomes a configure output; keep a committed fallback equal to Makevars.in's no-lib form so source installs without configure still build the flat path)

- [ ] **Step 1: Capture current Makevars and build the template.** Read the current `src/Makevars` first. Create `src/Makevars.in` = current content plus the two substitution points:

```
# src/Makevars.in - configure substitutes @DAFR_SHARD_CPPFLAGS@ / @DAFR_SHARD_LIBS@.
CXX_STD = CXX17
PKG_CPPFLAGS = @DAFR_SHARD_CPPFLAGS@
PKG_LIBS = @DAFR_SHARD_LIBS@
```
(Merge with any flags already in the existing `src/Makevars`; do not drop them.)

- [ ] **Step 2: `configure`** (POSIX sh; probe via `${R_HOME}/bin/R CMD config` compiler):

```sh
#!/bin/sh
# Probe optional system c-blosc / libzstd for packed ZarrDaf read.
: "${R_HOME=`R RHOME`}"
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
LDFLAGS=`"${R_HOME}/bin/R" CMD config LDFLAGS`

probe() { # header lib symbol -> echo "ok" on success
  cat > conftest.c <<EOF
#include <$1>
int main(void){ return 0; }
EOF
  if $CC $CFLAGS $CPPFLAGS conftest.c -o conftest $LDFLAGS -l$2 >/dev/null 2>&1; then
    echo ok; fi
  rm -f conftest conftest.c
}

SHARD_CPP=""; SHARD_LIBS=""
if [ -n "`probe blosc.h blosc`" ]; then
  SHARD_CPP="$SHARD_CPP -DHAVE_BLOSC"; SHARD_LIBS="$SHARD_LIBS -lblosc"
  echo "configure: c-blosc found - blosc-packed ZarrDaf read enabled"
else
  echo "configure: c-blosc NOT found - blosc-packed read will error at runtime"
fi
if [ -n "`probe zstd.h zstd`" ]; then
  SHARD_CPP="$SHARD_CPP -DHAVE_ZSTD"; SHARD_LIBS="$SHARD_LIBS -lzstd"
  echo "configure: libzstd found - zstd-packed ZarrDaf read enabled"
else
  echo "configure: libzstd NOT found - zstd-packed read will error at runtime"
fi

sed -e "s|@DAFR_SHARD_CPPFLAGS@|$SHARD_CPP|" \
    -e "s|@DAFR_SHARD_LIBS@|$SHARD_LIBS|" \
    src/Makevars.in > src/Makevars
```

- [ ] **Step 3: `configure.win`** (no system blosc/zstd on stock Windows; emit empty substitutions so the flat path builds):

```sh
#!/bin/sh
sed -e "s|@DAFR_SHARD_CPPFLAGS@||" -e "s|@DAFR_SHARD_LIBS@||" \
    src/Makevars.in > src/Makevars.win
```

- [ ] **Step 4: chmod + .Rbuildignore.**

Run: `chmod +x configure configure.win`
Add to `.Rbuildignore`: `^dev$`, `^src/Makevars$` is NOT ignored (it's the fallback). Ensure `^configure\.win$` etc. are shipped (do not ignore them).

- [ ] **Step 5: Commit a no-lib fallback `src/Makevars`** equal to `configure` output with empty flags, so `R CMD INSTALL` works even if `configure` is skipped.

```bash
git add src/Makevars.in configure configure.win src/Makevars .Rbuildignore
git commit -m "build(zarr-packed): configure-gated optional c-blosc/libzstd"
```

- [ ] **Step 6: Rebuild and verify the macro reaches the wrapper.**

Run: `cd /home/aviezerl/src/dafr-native && ./configure && Rscript -e 'devtools::load_all(quiet=TRUE); cat("blosc=",dafr:::dafr_have_blosc_cpp()," zstd=",dafr:::dafr_have_zstd_cpp(),"\n")'`
Expected (conda env has both): `blosc= TRUE zstd= TRUE`. (On a stock box: both FALSE - that is the CRAN path and must still build.)

---

## Task 5: crc32c R test (known vectors)

**Files:**
- Test: `tests/testthat/test-crc32c.R`

- [ ] **Step 1: Failing test** (the standard `"123456789"` crc32c check value is `0xE3069283`):

```r
test_that("crc32c matches the canonical check value", {
    x <- charToRaw("123456789")
    expect_equal(dafr:::dafr_crc32c_cpp(x), 0xE3069283)
})

test_that("crc32c of empty input is 0", {
    expect_equal(dafr:::dafr_crc32c_cpp(raw(0L)), 0)
})
```

- [ ] **Step 2: Run - expect PASS** (C core already built in Task 2-3).

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-crc32c.R")'`
Expected: 2 pass.

- [ ] **Step 3: Commit.**

```bash
git add tests/testthat/test-crc32c.R
git commit -m "test(zarr-packed): crc32c canonical check vectors"
```

---

## Task 6: Shard index parser (pure R) + detection helper

**Files:**
- Create: `R/zarr_sharded.R`
- Test: `tests/testthat/test-zarr-sharded.R`

- [ ] **Step 1: Failing test** for index parse against the committed gzip fixture (codec-agnostic: index layout is identical across codecs):

```r
test_that("zarr_sharded parses the start shard index of the score vector", {
    store <- new_dir_store(testthat::test_path(
        "fixtures/daf030-packed/gzip.daf.zarr"))
    node <- zarr_v3_read_array(store, "vectors/cell/score")
    expect_true(.zarr_is_sharded(node))
    shard <- store_get_bytes(store, zarr_v3_chunk_path("vectors/cell/score", 1L))
    cfg <- .zarr_sharding_config(node)
    idx <- .zarr_shard_index(shard, node, cfg)   # data.frame(offset, nbytes)
    expect_equal(nrow(idx), 2L)              # ceil(1200/1024)
    expect_equal(idx$offset[[1]], 89)
    expect_equal(idx$nbytes[[1]], 270)
    expect_equal(idx$offset[[2]], 412)
})
```

- [ ] **Step 2: Run - expect FAIL** ("could not find function .zarr_is_sharded").

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-zarr-sharded.R")'`

- [ ] **Step 3: Implement detection + index parse** in `R/zarr_sharded.R`:

```r
# R/zarr_sharded.R
# Read-only Zarr v3 packed/sharded arrays (ZEP-0002, DAF `packed=true`).
# Pure R except for inner-codec decompression (gzip via base R; blosc/zstd via
# optional configure-gated C in src/shard_codecs.cpp). Flat arrays never reach
# here - routed by R/zarr_format.R only when .zarr_is_sharded(node).

`%||%` <- function(a, b) if (is.null(a)) b else a

# TRUE if the array node's first codec is the sharding codec.
.zarr_is_sharded <- function(node) {
    cs <- node$codecs
    length(cs) >= 1L && identical(cs[[1L]]$name, "sharding_indexed")
}

# The sharding codec configuration list.
.zarr_sharding_config <- function(node) node$codecs[[1L]]$configuration

# Number of inner chunks per on-disk dimension and total N (C-order grid).
.zarr_shard_grid <- function(node, cfg) {
    outer <- as.integer(unlist(node$shape))
    inner <- as.integer(unlist(cfg$chunk_shape))
    per_dim <- as.integer(ceiling(outer / inner))
    list(outer = outer, inner = inner, per_dim = per_dim,
         n = prod(per_dim))
}

# Parse the start-located shard index: N*(offset:u64, nbytes:u64) LE then a
# 4-byte crc32c over the N*16 index bytes. Warns (does not stop) on crc
# mismatch - offsets remain usable. Returns a data.frame(offset, nbytes) with
# empty-chunk sentinels (0xFFFF..FF) mapped to NA.
.zarr_shard_index <- function(shard, node, cfg) {
    grid <- .zarr_shard_grid(node, cfg)
    n <- grid$n
    idx_bytes <- n * 16L
    if (length(shard) < idx_bytes + 4L) {
        stop("zarr_sharded: shard shorter than its start index", call. = FALSE)
    }
    have <- as.numeric(dafr_crc32c_cpp(shard[seq_len(idx_bytes)])) %% 2^32
    want <- readBin(shard[idx_bytes + 1:4], "integer", 1L, 4L,
                    endian = "little") %% 2^32
    if (!isTRUE(all.equal(have, want))) {
        warning(sprintf("zarr_sharded: shard index crc32c mismatch (%.0f != %.0f); reading anyway",
                        have, want), call. = FALSE)
    }
    # Each (offset, nbytes) is a u64 LE; R has no native u64, so read as u32
    # low/high pairs and recombine in doubles (values < 2^53 here, exact).
    con <- rawConnection(shard[seq_len(idx_bytes)], "rb")
    on.exit(close(con))
    u32 <- readBin(con, "integer", n * 4L, 4L, endian = "little")
    u32 <- ifelse(u32 < 0L, as.double(u32) + 2^32, as.double(u32))
    u64 <- u32[c(TRUE, FALSE)] + u32[c(FALSE, TRUE)] * 2^32  # n*2 values
    off <- u64[c(TRUE, FALSE)]
    nb  <- u64[c(FALSE, TRUE)]
    is_empty <- off >= 2^63   # 0xFFFF...FF sentinel -> all-fill chunk
    off[is_empty] <- NA_real_; nb[is_empty] <- NA_real_
    data.frame(offset = off, nbytes = nb)
}

# An n-length zero/empty vector of the R type for a v3 dtype (handles the
# integer64 case, which base `vector()` cannot allocate).
.zarr_zero_vector <- function(dtype, n) {
    k <- zarr_v3_r_kind_for_dtype(dtype)
    if (k == "integer64") return(bit64::as.integer64(rep(0L, n)))
    if (k == "character") return(character(n))
    vector(k, n)   # logical/integer/double -> FALSE/0L/0
}
```

Add `R/zarr_sharded.R` to the `Collate` if one is maintained (check `DESCRIPTION`); otherwise roxygen `@include` is unused here. Ensure `dafr_crc32c_cpp` is callable (it is, via cpp11).

- [ ] **Step 4: Run - expect PASS.**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-zarr-sharded.R")'`
Expected: 1 pass.

- [ ] **Step 5: Commit.**

```bash
git add R/zarr_sharded.R tests/testthat/test-zarr-sharded.R
git commit -m "feat(zarr-packed): detect sharded arrays + parse start shard index"
```

---

## Task 7: Inner-codec dispatch (gzip first - no dependency)

**Files:**
- Modify: `R/zarr_sharded.R`
- Test: `tests/testthat/test-zarr-sharded.R`

- [ ] **Step 1: Failing test** - decode one inner chunk of the gzip fixture's score vector to 1024 float64:

```r
test_that("zarr_sharded decodes a gzip inner chunk to its raw element bytes", {
    store <- new_dir_store(testthat::test_path(
        "fixtures/daf030-packed/gzip.daf.zarr"))
    node <- zarr_v3_read_array(store, "vectors/cell/score")
    cfg <- .zarr_sharding_config(node)
    shard <- store_get_bytes(store, zarr_v3_chunk_path("vectors/cell/score", 1L))
    idx <- .zarr_shard_index(shard, node, cfg)
    raw0 <- shard[idx$offset[[1]] + seq_len(idx$nbytes[[1]])]  # 1-based slice
    elem <- .zarr_inner_decode(raw0, cfg, n_elem = prod(cfg$chunk_shape[[1]]),
                               dtype = node$data_type)
    expect_equal(length(elem), 1024L)
    expect_equal(elem[1:3], c(1, 2, 3))   # score = 1:1200
})
```

- [ ] **Step 2: Run - expect FAIL** (".zarr_inner_decode not found").

- [ ] **Step 3: Implement inner-codec dispatch** (append to `R/zarr_sharded.R`):

```r
# The compression codec name in the inner pipeline (skip the leading `bytes`).
.zarr_inner_codec_name <- function(cfg) {
    for (c in cfg$codecs) if (!identical(c$name, "bytes")) return(c$name)
    "bytes"  # uncompressed inner (rare)
}

# Decompress one inner chunk's raw bytes to a numeric/integer/logical vector of
# exactly n_elem values for dtype. The `bytes` endian step is a no-op (LE).
.zarr_inner_decode <- function(raw_bytes, cfg, n_elem, dtype) {
    codec <- .zarr_inner_codec_name(cfg)
    elem_size <- zarr_v3_size_for_dtype(dtype)        # 8/4/1...
    out_nbytes <- n_elem * elem_size
    plain <- switch(codec,
        "gzip" = memDecompress(raw_bytes, type = "gzip"),
        "zstd" = dafr_zstd_decompress_cpp(raw_bytes, out_nbytes),
        "blosc" = dafr_blosc_decompress_cpp(raw_bytes, out_nbytes),
        "bytes" = raw_bytes,
        stop(sprintf("zarr_sharded: unsupported inner codec %s", sQuote(codec)),
             call. = FALSE))
    # Reuse the flat decoder for raw little-endian element bytes -> R vector.
    zarr_v3_decode_chunk(plain, dtype, n = n_elem)
}
```

- [ ] **Step 4: Run - expect PASS.**

- [ ] **Step 5: Commit.**

```bash
git add R/zarr_sharded.R tests/testthat/test-zarr-sharded.R
git commit -m "feat(zarr-packed): inner-codec dispatch (gzip base-R; blosc/zstd C)"
```

---

## Task 8: 1-D sharded reassembly (`.zarr_read_sharded_vector`)

**Files:**
- Modify: `R/zarr_sharded.R`
- Test: `tests/testthat/test-zarr-sharded.R`

- [ ] **Step 1: Failing test** - full score vector from the gzip fixture:

```r
test_that("zarr_sharded reassembles the full score vector (gzip)", {
    store <- new_dir_store(testthat::test_path(
        "fixtures/daf030-packed/gzip.daf.zarr"))
    node <- zarr_v3_read_array(store, "vectors/cell/score")
    v <- .zarr_read_sharded_vector(store, "vectors/cell/score", node)
    expect_equal(length(v), 1200L)
    expect_equal(v, as.numeric(1:1200))
})
```

- [ ] **Step 2: Run - expect FAIL.**

- [ ] **Step 3: Implement** (append). 1-D: inner chunks tile dim0; chunk `k` covers elements `k*inner .. min((k+1)*inner, len)-1`; decode to `inner` elems, take the valid prefix.

```r
# Read a sharded 1-D array (vector or a sparse component) to an R vector.
.zarr_read_sharded_vector <- function(store, base, node) {
    cfg <- .zarr_sharding_config(node)
    shard <- store_get_bytes(store, zarr_v3_chunk_path(base, 1L))
    if (is.null(shard)) stop(sprintf("sharded array %s missing chunk",
                                     sQuote(base)), call. = FALSE)
    idx <- .zarr_shard_index(shard, node, cfg)
    grid <- .zarr_shard_grid(node, cfg)
    len <- grid$outer[[1L]]; inner <- grid$inner[[1L]]
    dtype <- node$data_type
    out <- .zarr_zero_vector(dtype, len)
    for (k in seq_len(grid$per_dim[[1L]])) {
        lo <- (k - 1L) * inner
        valid <- min(inner, len - lo)
        if (is.na(idx$offset[[k]])) next            # empty chunk -> fill (0)
        raw_k <- shard[idx$offset[[k]] + seq_len(idx$nbytes[[k]])]
        chunk <- .zarr_inner_decode(raw_k, cfg, n_elem = inner, dtype = dtype)
        out[(lo + 1L):(lo + valid)] <- chunk[seq_len(valid)]
    }
    out
}
```

- [ ] **Step 4: Run - expect PASS.**

- [ ] **Step 5: Commit.**

```bash
git add R/zarr_sharded.R tests/testthat/test-zarr-sharded.R
git commit -m "feat(zarr-packed): reassemble 1-D sharded arrays"
```

---

## Task 9: 2-D sharded reassembly (`.zarr_read_sharded_matrix`)

**Files:**
- Modify: `R/zarr_sharded.R`
- Test: `tests/testthat/test-zarr-sharded.R`

- [ ] **Step 1: Failing test** - dense matrix from gzip fixture (Daf dims cell×gene = 1200×8):

```r
test_that("zarr_sharded reassembles a dense matrix column-major (gzip)", {
    store <- new_dir_store(testthat::test_path(
        "fixtures/daf030-packed/gzip.daf.zarr"))
    node <- zarr_v3_read_array(store, "matrices/cell/gene/dense")
    m <- .zarr_read_sharded_matrix(store, "matrices/cell/gene/dense", node)
    expect_equal(dim(m), c(1200L, 8L))
    expect_equal(as.numeric(m), as.numeric(1:(1200*8)))  # column-major fill
})
```

- [ ] **Step 2: Run - expect FAIL.**

- [ ] **Step 3: Implement.** On-disk `shape = [d0, d1]` (reversed: d0=n_cols, d1=n_rows). Build the flat on-disk C-order buffer (length d0*d1, fastest dim = d1), scatter each inner chunk, then `nr=d1, nc=d0`, `dim<-c(nr,nc)` (column-major = C-order of [d0,d1], identical to the flat path).

```r
# Read a sharded 2-D dense array to an R matrix (Daf dims nr=shape[2], nc=shape[1]).
.zarr_read_sharded_matrix <- function(store, base, node) {
    cfg <- .zarr_sharding_config(node)
    shard <- store_get_bytes(store, zarr_v3_chunk_path(base, 2L))
    if (is.null(shard)) stop(sprintf("sharded matrix %s missing chunk",
                                     sQuote(base)), call. = FALSE)
    idx <- .zarr_shard_index(shard, node, cfg)
    grid <- .zarr_shard_grid(node, cfg)
    d0 <- grid$outer[[1L]]; d1 <- grid$outer[[2L]]      # on-disk dims
    i0 <- grid$inner[[1L]]; i1 <- grid$inner[[2L]]
    n0 <- grid$per_dim[[1L]]; n1 <- grid$per_dim[[2L]]
    dtype <- node$data_type
    flat <- .zarr_zero_vector(dtype, d0 * d1)   # on-disk C-order buffer
    lin <- 0L
    for (c0 in seq_len(n0)) for (c1 in seq_len(n1)) {       # C-order grid
        lin <- lin + 1L
        lo0 <- (c0 - 1L) * i0; lo1 <- (c1 - 1L) * i1
        v0 <- min(i0, d0 - lo0); v1 <- min(i1, d1 - lo1)
        if (is.na(idx$offset[[lin]])) next
        raw_k <- shard[idx$offset[[lin]] + seq_len(idx$nbytes[[lin]])]
        chunk <- .zarr_inner_decode(raw_k, cfg, n_elem = i0 * i1, dtype = dtype)
        # chunk is C-order over [i0, i1]: element(a,b) at a*i1 + b.
        for (a in seq_len(v0)) {
            dst <- (lo0 + a - 1L) * d1 + lo1                # row (lo0+a) in [d0,d1]
            src <- (a - 1L) * i1
            flat[(dst + 1L):(dst + v1)] <- chunk[(src + 1L):(src + v1)]
        }
    }
    nr <- d1; nc <- d0
    dim(flat) <- c(nr, nc)   # column-major over (nr,nc) == C-order of [d0,d1]
    flat
}
```

- [ ] **Step 4: Run - expect PASS.** If `as.numeric(m)` is permuted, the per-chunk scatter math is the single point to fix - verify against this fixture before proceeding (Risk in spec §10).

- [ ] **Step 5: Commit.**

```bash
git add R/zarr_sharded.R tests/testthat/test-zarr-sharded.R
git commit -m "feat(zarr-packed): reassemble 2-D sharded dense matrices (column-major)"
```

---

## Task 10: Route sharded arrays from `R/zarr_format.R`

**Files:**
- Modify: `R/zarr_format.R` (dense-vector read ~L571-587; dense-matrix read `.zarr_get_dense_matrix` ~L859; sparse component reads `.zarr_get_sparse_vector` ~L605-629 and `.zarr_get_sparse_matrix` ~L892-936; mmap gate at L575, L871)
- Test: `tests/testthat/test-zarr-packed-read.R`

- [ ] **Step 1: Failing test** - end-to-end `get_vector` / `get_matrix` on the gzip fixture via `zarr_daf`:

```r
test_that("zarr_daf reads a packed (gzip) store end-to-end", {
    daf <- zarr_daf(testthat::test_path(
        "fixtures/daf030-packed/gzip.daf.zarr"), mode = "r")
    expect_equal(as.numeric(get_vector(daf, "cell", "score")),
                 as.numeric(1:1200))
    m <- get_matrix(daf, "cell", "gene", "dense")
    expect_equal(dim(m), c(1200L, 8L))
    expect_equal(as.numeric(m), as.numeric(1:(1200*8)))
    sm <- get_matrix(daf, "cell", "gene", "sparse")
    expect_s4_class(sm, "dgCMatrix")
    expect_equal(dim(sm), c(1200L, 8L))
})
```

- [ ] **Step 2: Run - expect FAIL** (flat reader misparses the shard - wrong values or error).

- [ ] **Step 3: Wire routing.** In the dense-vector reader, before the mmap/flat decode:

```r
    # Dense (array) path.
    if (.zarr_is_sharded(node)) {
        return(.zarr_read_sharded_vector(store, base, node))
    }
    n <- as.integer(node$shape[[1L]])
```
In `.zarr_get_dense_matrix`, at the top:
```r
.zarr_get_dense_matrix <- function(store, base, node) {
    if (.zarr_is_sharded(node)) {
        return(.zarr_read_sharded_matrix(store, base, node))
    }
    ...
```
In the sparse component readers, each component (`nzind`, `nzval`, `colptr`, `rowval`) is its own array node; decode it sharded-or-flat with a single helper. Add to `R/zarr_sharded.R`:
```r
# Decode a 1-D component array (sharded or flat) to an R vector. Used for
# sparse nzind/nzval/colptr/rowval, which shard independently above threshold.
.zarr_read_component_vector <- function(store, base, node) {
    if (.zarr_is_sharded(node)) return(.zarr_read_sharded_vector(store, base, node))
    n <- as.integer(node$shape[[1L]])
    chunk <- store_get_bytes(store, zarr_v3_chunk_path(base, 1L))
    zarr_v3_decode_chunk(chunk, node$data_type, n = n)
}
```
Then in `.zarr_get_sparse_vector` / `.zarr_get_sparse_matrix`, replace each
`store_get_bytes(...) |> zarr_v3_decode_chunk(...)` pair for a component with
`.zarr_read_component_vector(store, paste0(base, "/<comp>"), <comp>_meta)`.
(Keep the existing 1-based→0-based and integer64→integer conversions wrapping
the returned vector.)

In both dense readers gate the mmap fast path: `.zarr_try_mmap_dense` is only
reached when `!.zarr_is_sharded(node)` (the early returns above already ensure
this) - confirm no other call path mmaps a sharded chunk.

- [ ] **Step 4: Run - expect PASS** (gzip path needs no optional libs).

- [ ] **Step 5: Commit.**

```bash
git add R/zarr_format.R R/zarr_sharded.R tests/testthat/test-zarr-packed-read.R
git commit -m "feat(zarr-packed): route sharded arrays through the sharded reader"
```

---

## Task 11: Cross-codec read + crc + empty-chunk + flat-still-works

**Files:**
- Test: `tests/testthat/test-zarr-packed-read.R`

- [ ] **Step 1: Add tests** covering all codecs (blosc/zstd skip when the lib is absent), and that flat sub-threshold components in the same store still read:

```r
.skip_if_no_blosc <- function() testthat::skip_if_not(dafr:::dafr_have_blosc_cpp(),
                                                      "c-blosc not built in")
.skip_if_no_zstd  <- function() testthat::skip_if_not(dafr:::dafr_have_zstd_cpp(),
                                                      "libzstd not built in")

for (codec in c("blosc_zstd_bitshuffle", "blosc_lz4_bitshuffle", "zstd")) {
    local({
        cc <- codec
        test_that(paste0("zarr_daf reads packed store: ", cc), {
            if (startsWith(cc, "blosc")) .skip_if_no_blosc() else .skip_if_no_zstd()
            daf <- zarr_daf(testthat::test_path(
                sprintf("fixtures/daf030-packed/%s.daf.zarr", cc)), mode = "r")
            expect_equal(as.numeric(get_vector(daf, "cell", "score")),
                         as.numeric(1:1200))
            expect_equal(as.numeric(get_matrix(daf, "cell", "gene", "dense")),
                         as.numeric(1:(1200*8)))
            # flat string vector in the same packed store still reads:
            expect_identical(unname(get_vector(daf, "cell", "label"))[1:2],
                             c("v1", "v2"))
            sm <- get_matrix(daf, "cell", "gene", "sparse")
            expect_equal(sm[1, 1], 1)
        })
    })
}

test_that("packed read errors actionably when the lib is absent", {
    if (dafr:::dafr_have_blosc_cpp()) testthat::skip("c-blosc present")
    daf <- zarr_daf(testthat::test_path(
        "fixtures/daf030-packed/blosc_zstd_bitshuffle.daf.zarr"), mode = "r")
    expect_error(get_vector(daf, "cell", "score"), "requires c-blosc")
})
```

- [ ] **Step 2: Run.**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-zarr-packed-read.R")'`
Expected: all pass (blosc/zstd run in the conda env; the no-lib error test skips there).

- [ ] **Step 3: Commit.**

```bash
git add tests/testthat/test-zarr-packed-read.R
git commit -m "test(zarr-packed): cross-codec read, flat-coexistence, no-lib error"
```

---

## Task 12: Live interop round-trip (gated on env DAF >= 0.3.0)

**Files:**
- Test: `tests/testthat/test-zarr-packed-interop.R`

- [ ] **Step 1: Test** that generates a packed store live and round-trips values dafr-read vs Julia-written (skips when the Julia env or libs are missing):

```r
test_that("dafr reads a freshly Julia-written packed store (live)", {
    skip_on_cran()
    skip_if_not(.daf_jl_uses_zarr_v3())
    skip_if_not(dafr:::dafr_have_blosc_cpp(), "c-blosc not built in")
    out <- withr::local_tempdir("daf-packed-live-")
    res <- run_julia(c(
        "using DataAxesFormats, SparseArrays",
        "import DataAxesFormats.PackedFormat as PF",
        "PF.DAF_PACKED_COMPRESSION = :blosc_zstd_bitshuffle",
        sprintf('d = ZarrDaf(raw"%s/p.daf.zarr", "w"; packed=true)', out),
        'add_axis!(d, "cell", ["c$(i)" for i in 1:1500])',
        'set_vector!(d, "cell", "v", Float64.(1:1500))',
        'println("OK")'))
    skip_if_not(any(grepl("^OK$", res)), "julia write failed")
    daf <- zarr_daf(file.path(out, "p.daf.zarr"), mode = "r")
    expect_equal(as.numeric(get_vector(daf, "cell", "v")), as.numeric(1:1500))
})
```

- [ ] **Step 2: Run - expect PASS** (conda env).

Run: `NOT_CRAN=true Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-zarr-packed-interop.R")'`

- [ ] **Step 3: Commit.**

```bash
git add tests/testthat/test-zarr-packed-interop.R
git commit -m "test(zarr-packed): live interop round-trip vs Julia-written packed store"
```

---

## Task 13: Docs, env, NEWS, full verification

**Files:**
- Modify: `DESCRIPTION`, `environment.yml`, `NEWS.md`, `R/http_store.R`/`R/zarr_format.R` docstrings as needed

- [ ] **Step 1: `DESCRIPTION`** - add `SystemRequirements: C++17; optional libblosc and libzstd (for reading packed/sharded ZarrDaf v3 stores)`.

- [ ] **Step 2: `environment.yml`** - uncomment `- c-blosc`; add `- zstd`.

- [ ] **Step 3: `NEWS.md`** - new subsection under the current dev version:

```markdown
## ZarrDaf: packed/sharded v3 read

* Reading packed/sharded (`packed=true`) Zarr v3 `.daf.zarr` stores is now
  supported (read-only; dafr still writes flat). The shard index (ZEP-0002,
  start-located, crc32c-checked) is parsed in R; inner chunks decode via
  `gzip` (base R), or optional system **c-blosc** (`blosc_zstd_bitshuffle` /
  `blosc_lz4_bitshuffle`) and **libzstd** (`zstd`) probed by `configure`.
* CRAN-safe: with no c-blosc/libzstd present the flat path is unchanged and a
  blosc/zstd-packed read raises an actionable "install c-blosc/libzstd" error.
```
Remove the packed bullet from "Known limitations" (now supported, modulo the
optional-lib requirement, which the new subsection states).

- [ ] **Step 4: Full suite + R CMD check (no-lib path).**

Run: `NOT_CRAN=true Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_dir("tests/testthat", reporter="summary", stop_on_failure=FALSE)'`
Expected: 0 failures (blosc/zstd live tests run in the env; the no-lib error test skips).

Run a no-lib build check to protect CRAN: temporarily build with empty flags
(`R CMD INSTALL` after `sed` empties the Makevars subs, or set
`DAFR_SHARD_CPPFLAGS=`), and confirm flat tests pass and packed tests skip/err
as designed. Document the command used.

- [ ] **Step 5: Commit + (optional) document regen.**

```bash
Rscript -e 'devtools::document()'
git add DESCRIPTION environment.yml NEWS.md man R
git commit -m "docs(zarr-packed): SystemRequirements, environment.yml, NEWS"
```

---

## Self-review notes

- **Spec coverage:** Decisions C (read packed, never write) - Tasks 6-11. Decision D (configure-gated blosc) - Tasks 3-4. crc32c always-compiled - Task 2. zstd + gzip + blosc inner codecs - Task 7. Dense byte-order pin - Task 9. Sparse components shard independently - Task 10. CRAN-without-lib - Tasks 4, 11, 13. Fixtures - Task 1. mmap gate - Task 10.
- **Open verification (do at execution, not assumed):** (a) exact per-chunk scatter in Task 9 - pin against the fixture; (b) whether `zarr_v3_chunk_path(base,2L)` is `c/0/0` for the single outer chunk (it is, since outer chunk grid == shape) - confirm via fixture path existence in Task 1; (c) sparse `sparse` matrix in the fixture actually shards nzval (needs > 1024 nonzeros - the generator writes 2000); confirm in Task 1 by checking `matrices/cell/gene/sparse/nzval/zarr.json` codec name == `sharding_indexed`.
- **Isolation:** execute on a branch off `dev` (e.g. `zarr-v3-packed-read`), mirroring the flat-core port's `zarr-v3-port` branch; merge to `dev` when green.
