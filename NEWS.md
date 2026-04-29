# dafr 0.2.0 (development)

## Internal: per-item cache_group refactor (slice 14)

The internal format API now returns per-item cache classifications,
matching `DataAxesFormats.jl` v0.2.0 (upstream commit `49fbba1`).
**No user-visible behavior change.**

- Every backend `format_get_*` method (scalar/axis_array/vector/matrix)
  returns `list(value, cache_group)` instead of a bare value.
- Every backend `format_set_*` method returns the cache_group constant
  for the just-written value (or `NULL`) instead of `invisible()`.
- New exported character constants `MEMORY_DATA`, `MAPPED_DATA`,
  `QUERY_DATA` — accepted by `empty_cache(daf, clear = ...)` /
  `keep = ...` alongside the existing lowercase forms.
- The reader-level cache (`R/readers.R`) now consults the
  backend-returned cache_group when storing fresh reads, instead of
  hardcoding the `"memory"` tier. mmap-eligible reads on
  `files_daf` now correctly land in the `"mapped"` tier.
- Per-item classification: `files_daf` returns `MEMORY_DATA` for
  string/factor reads (R's CHARSXP cache makes mmap moot for strings)
  and `MAPPED_DATA` for everything else. Matches upstream's
  structural classification — no size thresholds.

This refactor is preparatory for the Slice 16+ `ZarrDaf` and
`HttpDaf` backends, which require per-item classification to drive
their internal caching.

# dafr 0.1.0 (development)

## Query DSL: Julia-parity parser additions

Three DataAxesFormats.jl query-DSL features that were previously
missing have been implemented, closing the last semantic gaps between
`dafr` and the upstream Julia package:

- `>> Reduction` reduces a vector or matrix to a scalar (e.g.
  `@ gene : is_lateral >> Sum type Int64`, or
  `@ cell @ gene :: UMIs >> Sum`). `>>` is no longer silently aliased
  to `>|`; on a grouped input (`... / g >> Sum`) it continues to
  produce a per-group vector, as before.
- `@ axis = entry` picks one entry from a vector
  (`@ cell : age @ cell = N89`) or one cell from a matrix
  (`@ cell @ gene :: UMIs @ cell = C @ gene = X`). Two successive
  picks collapse a matrix to a scalar.
- `|| value type T` attaches a Julia-style dtype (`Bool`,
  `Int8`..`Int64`, `UInt8`..`UInt64`, `Float32`, `Float64`, `String`)
  to a scalar-lookup default, matching the existing behaviour of the
  same suffix inside reductions and element-wise ops.
- `IfMissing()` builder gains an optional `type` kwarg
  (`IfMissing(0, type = "Int64")`).

# dafr 0.1.0

First public release.

A native R + C++ implementation of the [DataAxesFormats][daf] (DAF)
data model for multi-dimensional data along arbitrary axes, ported
from the [Julia reference implementation][daf] with no Julia
dependency.

[daf]: https://github.com/tanaylab/DataAxesFormats.jl

## Features

- **Core model.** Scalars, per-axis vectors, per-axis-pair matrices,
  axis entries, cache invalidation. `memory_daf()` and `files_daf()`
  backends.
- **Query DSL.** String form (`daf["@ cell : donor"]`) and pipe-chain
  builders (`daf[Axis("cell") |> LookupVector("donor") |> IsGreater(2)]`).
- **Memory-mapped reads** for vectors and sparse matrices from a
  read-only `files_daf()` store (zero-copy).
- **OpenMP-parallel C++ kernels** for Sum / Mean / Var / Mode /
  Quantile / GeoMean, with a CRAN-compliant 2-core auto-cap via
  `set_num_threads()`.
- **AnnData interop.** `DafAnnData` R6 facade, `as_anndata()`,
  `h5ad_as_daf()`, `daf_as_h5ad()` — sparse `X`, categorical
  `obs`/`var`, nested `uns`, and `obsm`/`varm` all round-trip.
- **dplyr backend.** `tbl(daf, axis)` → lazy `daf_axis_tbl` with
  `filter`, `select`, `mutate`, `arrange`, `summarise`, `group_by`,
  `distinct`, `pull`, `collect`, `compute` (write-back).
- **Contracts.** `create_contract()`, `verify_contract()`,
  `contract_scalar()` / `contract_vector()` / `contract_matrix()` /
  `tensor_contract()` / `axis_contract()` for computation
  pre/post-condition validation.

See the [pkgdown site](https://tanaylab.github.io/dafr/) for full
documentation.
