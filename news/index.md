# Changelog

## dafr 0.2.0 (development)

### ZarrDaf backend (slice 16)

- New `zarr_daf(uri, mode, name)` backend reading and writing Zarr v2.
  Two store impls: `DirStore` (filesystem directory tree) and
  `DictStore` (in-memory). Zip-backed Zarr (`MmapZipStore`) lands in
  slice 17.
- New `files_to_zarr(src, dst)` and `zarr_to_files(src, dst)` conversion
  helpers (same-filesystem only; correctness-first implementation
  re-encodes through the public API; hard-link optimization deferred as
  a perf follow-up).
- `open_daf("foo.daf.zarr")` now returns a `ZarrDaf`. The
  `.daf.zarr.zip` placeholder error now points to slice 17.
- Compression policy: dafr writes Zarr chunks uncompressed; reads
  uncompressed and gzip; rejects blosc/zstd/lz4 with a clear error
  pointing to re-save with `compressor=None`.
- Sparse layouts mirror upstream `DataAxesFormats.jl`: 1-based on-disk
  indices for `nzind` / `colptr` / `rowval`; sparse-Bool all-`TRUE`
  skips `nzval` (storage compaction). Cross-language parity is verified
  via gated Python `zarr.open()` smoke tests.
- Mirrors `DataAxesFormats.jl` v0.2.0 commits `ea4b5f9` (Zarr v2
  directory tree), `8cc3ff6` (in-memory store), `47e7693` (CRC fix — N/A
  for our in-memory layer), `79034fd` (`.zmetadata` consolidation),
  `46d4ab2` (Files↔︎Zarr conversion).

### reorder_axes() + open_daf() factory (slice 15)

- New `reorder_axes(daf, axis = perm, ...)` permutes axis entries in
  place, rewriting every vector and matrix that depends on the axis. On
  `files_daf` the operation is crash-recoverable via a
  `.reorder.backup/` directory of hardlinks; on the next
  `files_daf(path, mode = "r+" | "w+")` open, any in-progress reorder is
  automatically rolled back to the pre-reorder state.
- New `reset_reorder_axes(daf)` to manually trigger recovery (mostly
  redundant given the auto-recovery on open).
- New `open_daf(uri, mode, name)` factory function — dispatches on path
  / URL pattern. `memory://` (or no path) → `memory_daf`, filesystem
  path → `files_daf`. Future backends (`*.daf.zarr` and `http(s)://`)
  are stubbed with explicit error messages pointing to the slices that
  will land them (16 and 18 respectively). The factory replaces the
  previous filesystem-only `open_daf` from `R/complete.R`.
- Mirrors `DataAxesFormats.jl` v0.2.0 commits `90301ff`, `070bd34` (axis
  reordering) and `b40377f` (`open_daf` factory).

### Internal: per-item cache_group refactor (slice 14)

The internal format API now returns per-item cache classifications,
matching `DataAxesFormats.jl` v0.2.0 (upstream commit `49fbba1`). **No
user-visible behavior change.**

- Every backend `format_get_*` method (scalar/axis_array/vector/matrix)
  returns `list(value, cache_group)` instead of a bare value.
- Every backend `format_set_*` method returns the cache_group constant
  for the just-written value (or `NULL`) instead of
  [`invisible()`](https://rdrr.io/r/base/invisible.html).
- New exported character constants `MEMORY_DATA`, `MAPPED_DATA`,
  `QUERY_DATA` — accepted by `empty_cache(daf, clear = ...)` /
  `keep = ...` alongside the existing lowercase forms.
- The reader-level cache (`R/readers.R`) now consults the
  backend-returned cache_group when storing fresh reads, instead of
  hardcoding the `"memory"` tier. mmap-eligible reads on `files_daf` now
  correctly land in the `"mapped"` tier.
- Per-item classification: `files_daf` returns `MEMORY_DATA` for
  string/factor reads (R’s CHARSXP cache makes mmap moot for strings)
  and `MAPPED_DATA` for everything else. Matches upstream’s structural
  classification — no size thresholds.

This refactor is preparatory for the Slice 16+ `ZarrDaf` and `HttpDaf`
backends, which require per-item classification to drive their internal
caching.

## dafr 0.1.0 (development)

### Query DSL: Julia-parity parser additions

Three DataAxesFormats.jl query-DSL features that were previously missing
have been implemented, closing the last semantic gaps between `dafr` and
the upstream Julia package:

- `>> Reduction` reduces a vector or matrix to a scalar (e.g.
  `@ gene : is_lateral >> Sum type Int64`, or
  `@ cell @ gene :: UMIs >> Sum`). `>>` is no longer silently aliased to
  `>|`; on a grouped input (`... / g >> Sum`) it continues to produce a
  per-group vector, as before.
- `@ axis = entry` picks one entry from a vector
  (`@ cell : age @ cell = N89`) or one cell from a matrix
  (`@ cell @ gene :: UMIs @ cell = C @ gene = X`). Two successive picks
  collapse a matrix to a scalar.
- `|| value type T` attaches a Julia-style dtype (`Bool`,
  `Int8`..`Int64`, `UInt8`..`UInt64`, `Float32`, `Float64`, `String`) to
  a scalar-lookup default, matching the existing behaviour of the same
  suffix inside reductions and element-wise ops.
- [`IfMissing()`](https://tanaylab.github.io/dafr/reference/IfMissing.md)
  builder gains an optional `type` kwarg
  (`IfMissing(0, type = "Int64")`).

## dafr 0.1.0

First public release.

A native R + C++ implementation of the
[DataAxesFormats](https://github.com/tanaylab/DataAxesFormats.jl) (DAF)
data model for multi-dimensional data along arbitrary axes, ported from
the [Julia reference
implementation](https://github.com/tanaylab/DataAxesFormats.jl) with no
Julia dependency.

### Features

- **Core model.** Scalars, per-axis vectors, per-axis-pair matrices,
  axis entries, cache invalidation.
  [`memory_daf()`](https://tanaylab.github.io/dafr/reference/memory_daf.md)
  and
  [`files_daf()`](https://tanaylab.github.io/dafr/reference/files_daf.md)
  backends.
- **Query DSL.** String form (`daf["@ cell : donor"]`) and pipe-chain
  builders
  (`daf[Axis("cell") |> LookupVector("donor") |> IsGreater(2)]`).
- **Memory-mapped reads** for vectors and sparse matrices from a
  read-only
  [`files_daf()`](https://tanaylab.github.io/dafr/reference/files_daf.md)
  store (zero-copy).
- **OpenMP-parallel C++ kernels** for Sum / Mean / Var / Mode / Quantile
  / GeoMean, with a CRAN-compliant 2-core auto-cap via
  [`set_num_threads()`](https://tanaylab.github.io/dafr/reference/set_num_threads.md).
- **AnnData interop.** `DafAnnData` R6 facade,
  [`as_anndata()`](https://tanaylab.github.io/dafr/reference/as_anndata.md),
  [`h5ad_as_daf()`](https://tanaylab.github.io/dafr/reference/h5ad_as_daf.md),
  [`daf_as_h5ad()`](https://tanaylab.github.io/dafr/reference/daf_as_h5ad.md)
  — sparse `X`, categorical `obs`/`var`, nested `uns`, and `obsm`/`varm`
  all round-trip.
- **dplyr backend.** `tbl(daf, axis)` → lazy `daf_axis_tbl` with
  `filter`, `select`, `mutate`, `arrange`, `summarise`, `group_by`,
  `distinct`, `pull`, `collect`, `compute` (write-back).
- **Contracts.**
  [`create_contract()`](https://tanaylab.github.io/dafr/reference/create_contract.md),
  [`verify_contract()`](https://tanaylab.github.io/dafr/reference/verify_contract.md),
  [`contract_scalar()`](https://tanaylab.github.io/dafr/reference/contract-entries.md)
  /
  [`contract_vector()`](https://tanaylab.github.io/dafr/reference/contract-entries.md)
  /
  [`contract_matrix()`](https://tanaylab.github.io/dafr/reference/contract-entries.md)
  /
  [`tensor_contract()`](https://tanaylab.github.io/dafr/reference/tensor_contract.md)
  /
  [`axis_contract()`](https://tanaylab.github.io/dafr/reference/axis_contract.md)
  for computation pre/post-condition validation.

See the [pkgdown site](https://tanaylab.github.io/dafr/) for full
documentation.
