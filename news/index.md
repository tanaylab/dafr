# Changelog

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
