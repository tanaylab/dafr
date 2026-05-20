# AnnData interop

## DafAnnData facade

If you’ve been using `anndata` / `scanpy` / `Seurat`, the facade exposes
a Daf with familiar property names:

``` r

d <- example_cells_daf()
ann <- as_anndata(d)
ann$n_obs
#> [1] 856
ann$n_vars
#> [1] 683
head(ann$obs_names)
#> [1] "demux_07_12_20_1_AACAAGATCCATTTCA-1" "demux_07_12_20_1_AACGAAAGTCCAATCA-1"
#> [3] "demux_07_12_20_1_AAGACAAAGTTCCGTA-1" "demux_07_12_20_1_AGACTCATCTATTGTC-1"
#> [5] "demux_07_12_20_1_AGATAGACATTCCTCG-1" "demux_07_12_20_1_ATCGTAGTCCAGTGCG-1"
dim(ann$X)
#> [1] 856 683
```

The facade is read-only: `ann$X <- ...` errors. To modify data, write to
the underlying Daf: `set_matrix(d, ...)`.

## Loading h5ad

``` r

d <- h5ad_as_daf("path/to/file.h5ad")
```

Requires `hdf5r` (a Suggests dep): `install.packages("hdf5r")`.

## Writing h5ad

``` r

daf_as_h5ad(d, "out.h5ad", overwrite = TRUE)
```

## Fixture round-trip

``` r

fixture <- system.file("extdata", "small_test.h5ad", package = "dafr")
if (file.exists(fixture) && requireNamespace("hdf5r", quietly = TRUE)) {
    d <- h5ad_as_daf(fixture)
    cat("loaded:", daf_name(d), "\n")
    cat("n_obs:", length(axis_entries(d, "obs")), "\n")
    cat("n_vars:", length(axis_entries(d, "var")), "\n")
}
#> loaded: small_test 
#> n_obs: 50 
#> n_vars: 20
```

## What the h5ad readers translate

[`h5ad_as_daf()`](https://tanaylab.github.io/dafr/reference/h5ad_as_daf.md)
(and
[`daf_as_h5ad()`](https://tanaylab.github.io/dafr/reference/daf_as_h5ad.md)
in the other direction) cover:

- `/X` dense and sparse (`csr_matrix`, `csc_matrix`) - sparse is
  round-tripped via
  [`Matrix::sparseMatrix`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html).
- `/obs` and `/var` columns: plain datasets and `categorical` groups
  (read as `factor`, ordered or not).
- `/layers` - dense layers become extra `obs x var` matrices keyed by
  the layer name.
- `/obsm` and `/varm` dense matrices - each is placed on a synthetic
  axis named `obsm_<name>_dim` / `varm_<name>_dim`.
- `/uns` scalars (flat and nested; nested keys are flattened with `_`).

Things still NOT translated (escalated via `unsupported_handler`,
default `WARN_HANDLER`, then skipped):

- `/obsp` and `/varp` (square per-pair matrices on obs/var).
- `/raw` (the legacy raw counts slot).
- Sparse layers / `obsm` / `varm` entries; only dense forms are read.
- Group-valued obs/var columns other than `categorical`.

If a strict round-trip matters, pass
`unsupported_handler = ERROR_HANDLER` so unhandled entries hard-fail
instead of being dropped silently.
