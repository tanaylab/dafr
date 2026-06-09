# Kickoff: ZarrDaf v3 follow-ups (2026-06-09)

Continuation of `2026-06-09-daf-0.3.0-followups-kickoff.md`. That doc's **Item 1
("re-fix zarr for 0.3.0")** is now partly shipped: the **flat core** of the Zarr
v2 -> v3 port landed. This doc records what shipped and the prioritized remainder
(the rest of Item 1, plus Items 2-4 from the parent doc, plus two new gaps the
port surfaced).

## What just shipped (this batch)

Branch `zarr-v3-port` (27 commits) merged to `dev` (`a87e9c3`). dafr's ZarrDaf
now reads **and** writes the **Zarr v3** on-disk format of DataAxesFormats.jl
0.3.0, for the **flat (uncompressed)** case. Version bumped 0.3.1 -> **0.4.0**.

- **New codec layer `R/zarr_v3.R`** (replaces deleted `R/zarr_v2.R`): per-node
  `zarr.json`; lowercase v3 dtype names (`int32`/`float64`/`int64`/`bool`/`string`,
  read also accepts `float32`/`int8-16`/`uint8-32`/`uint64`); `c/`-prefixed chunk
  keys (`c/0`, `c/0/0`); daf marker as the **root-group attribute** `{"daf":[1,0]}`;
  inline `consolidated_metadata` (kind `inline`) at the root.
- **`R/zarr_format.R`** read+write fully on v3 (scalars, axes, dense+sparse
  vectors, dense+sparse matrices, strings, all dtypes; mmap zero-copy fast path
  ported to `float64`/`int32` + `c/0` keys).
- **Clean break:** v2 stores are **rejected on open** with a `python -m zarr
  v2_to_v3` hint (mirrors DAF 0.3.0). `R/zarr_v2.R` + its tests deleted.
- **Interop verified live** against DAF 0.3.0 (both directions, negative-control
  checked): `tests/testthat/test-zarr-julia-interop.R` (31 expectations, runs when
  env DAF >= 0.3.0). Full suite green (`NOT_CRAN=true`: 5876 pass / 0 fail / 131
  env-gated skips); `R CMD check` 0 errors (warnings/notes pre-existing).
- **Spec + plan** committed: `docs/superpowers/specs/2026-06-09-zarrdaf-v3-port-design.md`,
  `docs/superpowers/plans/2026-06-09-zarrdaf-v3-flat-core.md`.

### Verified ground-truth facts (don't re-derive these)
- **Default DAF 0.3.0 writes are FLAT** (single uncompressed `bytes` chunk).
  Packing is **opt-in** via `ZarrDaf(path; packed=true)`; threshold is per-column
  `DAF_PACKED_TARGET_CHUNK_KB = 8` KB. So the flat core reads/writes real
  default-written stores fully; packed is the uncommon case.
- **Dense matrix on disk: shape REVERSED `[n_cols, n_rows]`**, chunk bytes
  column-major (verified from a real fixture). Reader uses `nr=shape[[2]]`,
  `nc=shape[[1]]`, fills `dim=c(nr,nc)` directly. No `order` field in v3.
- **Sparse CSC `colptr`/`rowval` and sparse-vector `nzind` are `int64`, 1-based**
  (DAF; dafr v2 used int32). All-true Bool sparse omits the `nzval` child.
- Packed/sharded uses **classic blosc1 chunks** (Zarr.jl -> Blosc.jl -> c-blosc
  1.x), default codec `blosc(zstd, bitshuffle)`; ZEP-0002 sharding, shard index at
  `:start`, crc32c-protected; the dual ZIP framing stores blosc bytes as ZIP
  method-0 (no shortcut around blosc decode).

## Environment / repo state
- conda `dafr-mcview` env has DAF 0.3.0 + `Zarr` 0.10 (Julia `1.12`); `~/src/
  DataAxesFormats.jl` @ 0.3.0 (`8541a4b`). Julia bin
  `/home/aviezerl/miniconda3/envs/dafr-mcview/bin/julia`, project env
  `.../share/julia/environments/dafr-mcview`. **Note:** a fresh-write `close()` on
  a ZarrDaf can throw a benign finalizer error in this env; the on-disk store is
  complete regardless (DirStore writes are eager).
- `dev` tracks `private/dev`; `origin/main` gitignores `dev/`. The merge did not
  push.
- Interop round-trips need `NOT_CRAN=true` (or `devtools::test()`) to run; a bare
  `Rscript test_file()` silently skips them via `skip_on_cran()`.

## Remaining work (prioritized)

### 1. Packed/sharded v3 READ (large; the rest of Item 1)
The decided scope (maintainer): **read** packed stores (in addition to flat),
**never write** packed; **CRAN-safe** compression backend. Backend decision was
adjusted from "vendor c-blosc" to **`configure`-gated optional system c-blosc**
to keep CRAN clean (vendoring full c-blosc trips CRAN's ~5 MB size NOTE + build
portability; the precedent that vendors it, Bioconductor's `Rarr`, is not on
CRAN). See spec phases 5-6 for the full design. Work:
- `configure`/`configure.win` probing for system c-blosc (`blosc.h` + `-lblosc`);
  `HAVE_BLOSC` -> thin `blosc_decompress` cpp11 wrapper, else a stub that errors
  "install c-blosc to read packed ZarrDaf". Add `c-blosc` to `environment.yml`
  (the commented line is already there).
- `crc32c` (Castagnoli, != zlib crc32): small table-based C, always compiled, for
  the shard index.
- `R/zarr_sharded.R` (new): detect `sharding` codec in an array's `codecs`; parse
  the start-located shard index (u64 offset/nbytes pairs); per inner chunk run the
  reverse pipeline (decompress blosc/zstd/gzip -> bytes/endian -> vlen-utf8 if
  string); reassemble per chunk grid (DAF dense matrices use one-column-per-inner-
  chunk). Sparse components decode independently.
- Fixtures: generate `packed=true` DAF stores from Julia (blosc_zstd_bitshuffle,
  blosc_lz4_bitshuffle, zstd, gzip inner codecs; dense + sparse).
- Write a fresh plan first (`docs/superpowers/plans/`), then subagent-driven.
- **CRAN stays green without blosc** (flat path intact; packed read errors with an
  actionable message on a no-blosc build).

### 2. HttpStore-over-v3 (medium; NEW gap the port surfaced)
`R/http_store.R` (the `zarr_daf("http://...")` ZarrDaf-over-HTTP backend) still
expects the v2 `.zmetadata` index, so reading a v3 `.daf.zarr` over HTTP fails on
open. This was **inherited** when the writer went v3 (not a regression from this
batch) and is now **documented as a known limitation** (NEWS + `http_store.R`
docstrings) and **pinned by a test** (`test-http-live.R`: `zarr_daf()` over HTTP
rejects a v3 store). Port: have `new_http_store()` GET the root `zarr.json`, parse
its inline `consolidated_metadata.metadata` as the node index (replacing the v2
`.zmetadata` dict), and resolve chunk GETs by v3 keys. **Not** affected and needs
no work: `HttpDaf`/`FilesDaf`-over-HTTP (the FilesFormat path, `R/http_format.R`),
which uses `metadata.zip` and is fully working/tested.

### 3. O(N^2) consolidated-metadata refresh (small perf)
`zarr_v3_write_consolidated` (`R/zarr_v3.R`) re-scans + re-parses the whole store
on **every** set/delete (DAF consolidates once at close). Correct + idempotent,
but O(N^2) for bulk writes. Comment in the code flags it. Fix: consolidate
lazily/at-close instead of per-mutation (needs a close/flush hook in the ZarrDaf
lifecycle). Also: a genuine `int64`/`uint64` non-bool sparse `nzval` is narrowed
to double on read (lossy > 2^53; DAF sparse values are realistically float/bool) -
commented at the decode site; revisit only if a real int64-valued sparse appears.

### 4. Item 2 (parent doc): Phase-2 dense compute kernels (medium)
Unchanged from the parent kickoff. Dense reductions + eltwise ops are single-
threaded vs Julia's parallel; benchmarked 2-6x gap. **Profile the `get_query` path
first** - the `% Log` 6.4x gap was mostly per-call alloc/cache/dispatch overhead
(~240 ms on a warm matrix), not the kernel. Reducing result-allocation / cache-
store overhead may be the bigger lever than adding kernels.

### 5. Item 3 (parent doc): FilesFormat 1.1 write + packed (medium)
Unchanged. dafr reads 1.1 / writes 1.0 (1.0 is 0.3.0-readable). Decide whether to
also write 1.1. Packed (`.zip`, chunked+compressed) FilesFormat components are
rejected; reading them is "bells and whistles" - revisit only if real repos use
them. (Note: the same vendored/configure blosc decode from item 1 would unlock
packed FilesFormat read too.)

### 6. Item 4 (parent doc): full 0.3.0 changelog audit (small-medium)
Unchanged. This batch covered 0.3.0's zarr (v2->v3 flat) + FilesFormat 1.1. The
rest of `b40377f..8541a4b` (parallel-loop policy, small fixes, packing WIP) hasn't
been diffed for non-format behavioural changes worth porting. Run the commit-by-
commit review.

## Suggested order
2 (HttpStore) and 3 (O(N^2)) are cheap and close real gaps the port left -
knock out first. Then 1 (packed read) is the big remaining piece of "re-fix zarr",
and it also de-risks 5 (packed FilesFormat shares the blosc backend). 4 and 6 are
independent and can slot in anytime.

## References
- Shipped: spec `docs/superpowers/specs/2026-06-09-zarrdaf-v3-port-design.md`,
  plan `docs/superpowers/plans/2026-06-09-zarrdaf-v3-flat-core.md`, `NEWS.md`
  (0.4.0 entry + known limitations).
- v3 tests: `tests/testthat/test-zarr-v3.R`, `test-zarr-v3-read.R`,
  `test-zarr-v3-write.R`, `test-zarr-julia-interop.R`; fixture
  `tests/testthat/fixtures/daf030-flat.daf.zarr`.
- Upstream: `~/src/DataAxesFormats.jl` `src/{zarr_format,packed_format}.jl` @
  `8541a4b` (0.3.0); `Zarr.jl` 0.10 + `Blosc.jl` (c-blosc 1.x).
- Parent: `dev/notes/2026-06-09-daf-0.3.0-followups-kickoff.md`.
