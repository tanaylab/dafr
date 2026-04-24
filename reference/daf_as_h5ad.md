# Write a Daf to a Muon-style h5ad file.

Inverse of
[`h5ad_as_daf()`](https://tanaylab.github.io/dafr/reference/h5ad_as_daf.md).
Writes `/X` (dense or CSC-sparse), per-axis column groups `/obs` and
`/var` (each with an `_index`; factor vectors emitted as h5ad
categorical groups), `/layers` for additional matrices, and flat `/uns`
scalars.

## Usage

``` r
daf_as_h5ad(
  daf,
  path,
  obs_axis = NULL,
  var_axis = NULL,
  x_name = "UMIs",
  overwrite = FALSE,
  unsupported_handler = WARN_HANDLER
)
```

## Arguments

- daf:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md).

- path:

  Destination `.h5ad` path.

- obs_axis:

  Axis name mapped to `/obs`. If `NULL`, auto-detected from `"cell"`,
  `"metacell"`.

- var_axis:

  Axis name mapped to `/var`. If `NULL`, auto-detected from `"gene"`.

- x_name:

  Matrix name (on the `obs_axis, var_axis` pair) written as `/X`.
  Default `"UMIs"`.

- overwrite:

  If `FALSE` (default), error when `path` already exists; if `TRUE`,
  silently replace.

- unsupported_handler:

  Handler for Daf features we cannot represent in h5ad. See
  [`inefficient_action_handler()`](https://tanaylab.github.io/dafr/reference/inefficient_action_handler.md).

## Value

Invisibly, `path`.

## Details

Note: the write side always emits `/uns` flat. On read we flatten nested
uns groups with a `_` separator, so a round-trip of a nested uns file
produces flat dotted keys on the resulting Daf.

Requires the `hdf5r` and `Matrix` packages.

## See also

[`h5ad_as_daf()`](https://tanaylab.github.io/dafr/reference/h5ad_as_daf.md),
[DafAnnData](https://tanaylab.github.io/dafr/reference/DafAnnData.md),
[`as_anndata()`](https://tanaylab.github.io/dafr/reference/as_anndata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2"))
add_axis(d, "gene", c("g1", "g2"))
set_matrix(d, "cell", "gene", "UMIs", matrix(1:4, 2, 2))
p <- tempfile(fileext = ".h5ad")
daf_as_h5ad(d, p, obs_axis = "cell", var_axis = "gene")
} # }
```
