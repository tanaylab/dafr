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
