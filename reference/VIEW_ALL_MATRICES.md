# Default view spec: expose all matrices as-is.

A pre-built `data` item for
[`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md) that
exposes every matrix of the base daf unchanged.

## Usage

``` r
VIEW_ALL_MATRICES
```

## Format

An object of class `list` of length 1.

## See also

[`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md),
[ALL_MATRICES](https://tanaylab.github.io/dafr/reference/ALL_MATRICES.md)

## Examples

``` r
d <- example_cells_daf()
v <- viewer(d, data = VIEW_ALL_MATRICES)
matrices_set(v, "cell", "gene")
#> [1] "UMIs"
```
