# Default view spec: expose all vectors as-is.

A pre-built `data` item for
[`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md) that
exposes every vector of every axis of the base daf unchanged.

## Usage

``` r
VIEW_ALL_VECTORS
```

## Format

An object of class `list` of length 1.

## See also

[`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md),
[ALL_VECTORS](https://tanaylab.github.io/dafr/reference/ALL_VECTORS.md)

## Examples

``` r
d <- example_cells_daf()
v <- viewer(d, data = VIEW_ALL_VECTORS)
vectors_set(v, "cell")
#> [1] "donor"      "experiment"
```
