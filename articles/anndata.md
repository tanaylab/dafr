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

## Limitations (0.1.0)

- Sparse matrix h5ad encoding (`csr_matrix` / `csc_matrix`) not yet
  translated; read path warns and skips.
- Categorical (factor) columns skipped on read.
- Nested `uns` groups skipped.
- `varm` / `obsm` / `obsp` / `varp` / `raw` not translated.
