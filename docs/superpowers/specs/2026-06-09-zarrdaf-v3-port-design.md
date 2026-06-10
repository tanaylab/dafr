# Design: ZarrDaf Zarr v2 → v3 port (DAF 0.3.0 interop)

Date: 2026-06-09
Status: approved-pending-review
Scope: dafr-native (`/home/aviezerl/src/dafr-native`), branch `zarr-v3-port` off `dev`

## 1. Problem & goal

DataAxesFormats.jl 0.3.0 moved its `ZarrDaf` (`.daf.zarr`) on-disk format from
Zarr **v2** to Zarr **v3**, and *rejects v2 stores on open*. dafr-native's
ZarrDaf is entirely Zarr v2 (`R/zarr_v2.R`, plus v2 assumptions threaded through
`R/zarr_format.R`). R↔Julia zarr interop therefore holds only against DAF ≤ 0.2.x;
the three round-trip tests skip when the env DAF ≥ 0.3.0 (`.daf_jl_uses_zarr_v3`).

**Goal:** port ZarrDaf to Zarr v3 so dafr interoperates with DAF 0.3.0.

## 2. Decisions (resolved with maintainer)

| # | Decision | Choice |
|---|----------|--------|
| A | Read/write scope | **v3 flat read + write** in this slice |
| B | Old dafr 0.2.x (v2) stores | **Clean break, v3 only** — drop the v2 reader; reject v2 on open |
| C | Packed/sharded arrays | **Read** them (in addition to flat); never write packed |
| D | Blosc backend | **`configure`-gated optional system c-blosc** (not vendored) — see §6, driven by the CRAN constraint below |

**CRAN constraint (overrides the earlier "vendor c-blosc" lean).** dafr targets
CRAN (`slice-21-cran-prep`). The flat path needs *zero* new dependencies and is
cleanly CRAN-compatible. Vendoring full c-blosc (bundled zstd ~3MB, lz4, zlib,
snappy) would trip CRAN's 5MB installed-size NOTE and multi-platform compile
scrutiny; the precedent that vendors c-blosc for Zarr (`Rarr`) lives on
Bioconductor, not CRAN. Therefore blosc/sharded read is a **compile-time optional
capability** gated by `configure`: present where a system blosc is installed
(conda env, r-universe, Bioc-style), absent (with an actionable runtime error) on
a stock CRAN build. Since **default DAF writes are flat**, the gated path is only
the uncommon `packed=true` case.

### Non-goals (explicitly out of scope)
- Writing packed/sharded or compressed v3 (we write flat uncompressed only).
- Reading legacy Zarr v2 dafr stores (clean break; `python -m zarr v2_to_v3` is the migration path, mirroring DAF's own error).
- Bundling/vendoring a compression library.

## 3. Zarr v3 format reference (the spec we port against)

Derived from `DataAxesFormats.jl` `src/{zarr_format,packed_format}.jl` @ 0.3.0
(commit ~`8541a4b`), which uses `Zarr.jl` 0.10 → `Blosc.jl` (classic c-blosc 1.x).

**Per-node metadata = single `zarr.json`** (replaces v2 `.zarray`/`.zgroup`/`.zattrs`/`.zmetadata`).

Group:
```json
{ "zarr_format": 3, "node_type": "group" }
```
Root group additionally carries:
```json
{ "zarr_format": 3, "node_type": "group",
  "attributes": { "daf": [1, 0] },
  "consolidated_metadata": { "kind": "inline", "must_understand": false,
                             "metadata": { "<rel/path>": <full node metadata>, ... } } }
```
- daf version marker = **root group attribute** `daf: [major, minor]` = `[1, 0]` (integers).
- `consolidated_metadata` is present in directory stores (rebuilt by DAF on open), omitted in `.zip` stores.

Array (flat, uncompressed — what we write and the common read case):
```json
{ "zarr_format": 3, "node_type": "array",
  "shape": [d0, d1, ...],
  "data_type": "<v3 name>",
  "chunk_grid": { "type": "regular", "configuration": { "chunk_shape": [d0, d1, ...] } },
  "chunk_key_encoding": { "name": "default", "configuration": { "separator": "/" } },
  "fill_value": <zero | "">,
  "codecs": [ { "name": "bytes", "configuration": { "endian": "little" } } ],
  "attributes": {} }
```
- String arrays add a `vlen-utf8` codec; `data_type` = `"string"`, `fill_value` = `""`.
- **dtype names (v3, lowercase)**: `bool`, `int8/16/32/64`, `uint8/16/32/64`, `float32`, `float64`, `string` (vs v2 numpy `<i4`, `|u1`, …). Mirror DAF's `typestr3` / `DTYPE_BY_NAME`.
- **chunk keys**: prefix `c/`, separator `/` → 1-D single chunk `c/0`, 2-D single chunk `c/0/0`.

Sparse (structurally identical to v2; each component is a v3 array):
```
vectors/<axis>/<prop>/            (group)  nzind/ , nzval/        (arrays; nzval omitted if Bool all-true)
matrices/<rows>/<cols>/<prop>/    (group)  colptr/ , rowval/ , nzval/   (CSC; nzval omitted if Bool all-true)
```
Indices are 1-based on disk (Julia convention), converted to 0-based on read — unchanged from v2.

Packed/sharded array (read-only; opt-in `packed=true` in DAF, **not** default):
- Detected by a `sharding` codec in the array's `codecs` (authoritative; `daf_packed_format:"indexed+zipped"` attribute is an additional hint).
- Sharding config: inner `chunk_shape`, inner `codecs` (`bytes` + compression), `index_codecs` (`bytes` + `crc32c`), `index_location: "start"`.
- Shard blob lives at `c/0` (1-D) / `c/0/0` (2-D). Index at start: per-inner-chunk `(offset:u64, nbytes:u64)` little-endian, crc32c-protected.
- Default inner compression = `blosc(cname="zstd", shuffle=BITSHUFFLE)` → **classic blosc1 chunk** (16-byte header, shuffle flagged internally). Also possible: `blosc(lz4, bitshuffle)`, plain `zstd`, `gzip`.
- Dense matrices: inner chunk = one column `(nrows, 1)` → natural column-major reassembly. The "dual ZIP framing" stores blosc bytes as ZIP method-0 (STORED), so it offers no shortcut around blosc decode.

## 4. Architecture

### Reuse unchanged
- **`R/zarr_store.R`** — `DirStore`/`DictStore`/`MmapZipStore` + `store_get/set/delete/exists/list` generics. Format-agnostic.
- **vlen-utf8** wire codec — identical in v2/v3.
- **Sparse group structure** + 1-based↔0-based index conversion.
- **cpp11 C++ layer** (`src/altrep_mmap*.cpp`, `src/altrep_zip_raw.cpp`, kernels) — keep; extend for optional blosc + crc32c + sharded byte-range reads.

### Remove (clean break)
- **`R/zarr_v2.R`** deleted.
- v2-specific code in `R/zarr_format.R`: `.zarray`/`.zgroup`/`.zattrs`/`.zmetadata` read/write, dotted chunk keys, numpy dtype strings, the `daf/0` array marker.
- Opening a v2 store **rejects** with a clear error (mirrors DAF): "Zarr v2 store not supported; dafr reads/writes Zarr v3 (DAF 0.3.0). Convert via `python -m zarr v2_to_v3 <path>`, then reopen."

### New
- **`R/zarr_v3.R`** — format codec layer. Replaces `zarr_v2.R`.
  - Read `zarr.json` → group vs array; for arrays parse shape, `data_type`, `chunk_grid.configuration.chunk_shape`, `chunk_key_encoding`, `fill_value`, `codecs`, `attributes`.
  - Write (flat): group `zarr.json` (root adds `daf` attr + consolidated metadata); array `zarr.json` per §3.
  - dtype map (v3 lowercase ↔ R types; reuse the v2 reader's existing per-dtype decode logic, only the name strings change).
  - chunk-key construction/parse parameterized from `chunk_key_encoding`.
  - consolidated-metadata builder for write; **on read, ignore consolidated metadata and walk the tree** (robust — zip stores omit it, DAF rebuilds on open).
- **`R/zarr_sharded.R`** — packed/sharded **read** only. Sharding-codec parse → shard-index parse (start, crc32c-checked) → per-inner-chunk reverse pipeline (decompress → bytes/endian → vlen-utf8 if string) → reassemble per chunk grid. Each sparse component decodes independently (flat or sharded).
- **`R/zarr_format.R`** (refactor) — drop v2 calls; route leaf metadata/chunk-key/dtype through `zarr_v3.R`; keep sparse/dense structural logic; version-detect on open; reject v2.
- **`R/zarr_convert.R`** — update dtype conversions to v3 names.
- **`src/` additions** — see §6.

## 5. mmap zero-copy fast path

v3 flat dense = single uncompressed `c/0`[/0] little-endian chunk = exactly what
`.zarr_try_mmap_dense` already handles. Port by swapping chunk-key construction
(`0`/`0.0` → `c/0`/`c/0/0`); keep the DirStore + uncompressed + `float64`/`int32`
conditions. Zero-copy reads survive the port for flat dense.

## 6. Compression backend (CRAN-safe, `configure`-gated)

- `configure` / `configure.win` probes for a system c-blosc (`blosc.h` + `-lblosc`).
  - Found → define `HAVE_BLOSC`, compile `src/blosc_r.cpp` (thin `blosc_decompress` wrapper; one call handles header + sub-codec + bit/byte-shuffle, matching what Julia writes). Link `-lblosc`.
  - Not found → compile a stub: any attempt to decode a blosc inner chunk raises "Reading packed/sharded ZarrDaf requires c-blosc; install it (e.g. `conda install c-blosc`) and reinstall dafr."
- **Inner codecs** for sharded read: `blosc` → optional system c-blosc; `zstd` (plain) → optional system libzstd (already present in env) or c-blosc's zstd; `gzip` → base-R `memDecompress`; `bytes`/vlen-utf8 → existing R.
- **crc32c** (Castagnoli; ≠ zlib crc32) → small table-based C, **always compiled** (no dep). Validate the shard index; on mismatch **warn** (offsets still usable), don't hard-fail.
- `environment.yml` gains `c-blosc` so the conda/lab path gets full sharded read. `DESCRIPTION` `SystemRequirements` notes optional libblosc. `R CMD check` passes without it (flat path intact).

## 7. Verification (fixture-driven — assume nothing)

- **Dense matrix byte order**: v2 reversed shape with `order=C`; v3 has no `order` field and DAF writes Julia column-major bytes. Whether R's native column-major maps directly or needs a dim handling step is **pinned by a real DAF-0.3.0 fixture round-trip**, not reasoned about abstractly.
- All read tests run off **committed DAF-0.3.0-written fixtures** (flat + `packed=true`, generated from Julia). Write tests have **Julia open dafr-written stores** and validate.
- Flip the skip guard: interop round-trips **skip when env DAF < 0.3.0** (run when ≥ 0.3.0).

## 8. Test plan

- v3 flat read/write round-trip: scalars; axes; dense/sparse vectors; dense/sparse matrices; strings; every dtype; empty arrays; Bool all-true sparse (nzval omitted).
- v3 packed/sharded read vs `packed=true` fixtures: `blosc_zstd_bitshuffle`, `blosc_lz4_bitshuffle`, plain `zstd`, `gzip` inner codecs; dense + sparse.
- Unit: blosc decode (known blosc1 buffers); crc32c (known vectors); mmap fast path on v3 flat dense; v2-store rejection error.
- `configure`-absent build: flat tests pass; opening a packed store yields the actionable error; packed tests skip.

## 9. Implementation phasing (→ writing-plans)

1. `zarr_v3.R` metadata/dtype/chunk-key/fill-value layer + DictStore unit tests (no Julia).
2. Flat **read** vs committed DAF flat fixtures (incl. the dense byte-order pin).
3. Flat **write** + consolidated metadata + v2 rejection; re-enable interop round-trips (flip guard).
4. mmap fast-path port (v3 flat dense).
5. `configure` + optional system blosc + crc32c build integration + cpp11 wrappers + unit tests.
6. Sharded **read** (`zarr_sharded.R`): dense then sparse, vs `packed=true` fixtures.
7. Cleanup: delete `zarr_v2.R`; NEWS/vignette/`environment.yml`/version bump; docs.

Each phase is independently testable; 1–4 are pure-R and CRAN-clean; 5–6 add the optional blosc capability.

## 10. Risks

- **Dense byte-order convention** (mitigated: fixture round-trip in phase 2 before building on it).
- **blosc1 chunk variants** (shuffle vs bitshuffle, blocksize edge cases) — covered by decoding real `packed=true` fixtures, not synthetic assumptions.
- **`configure` portability** (Windows `configure.win`; graceful no-blosc fallback) — the no-blosc path must build and pass check everywhere.
- **Consolidated-metadata faithfulness** — verify DAF opens dafr-written stores both with and without it; we write it to be safe.
