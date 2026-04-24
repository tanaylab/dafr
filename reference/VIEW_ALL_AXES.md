# Default view spec: expose all axes as-is.

A pre-built `axes` override list for
[`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md) that
exposes every axis of the base daf unchanged. Equivalent to
`list(list(ALL_AXES, "="))`.

## Usage

``` r
VIEW_ALL_AXES
```

## Format

An object of class `list` of length 1.

## See also

[`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md),
[ALL_AXES](https://tanaylab.github.io/dafr/reference/ALL_AXES.md)

## Examples

``` r
d <- example_cells_daf()
v <- viewer(d, axes = list(VIEW_ALL_AXES))
axes_set(v)
#> [1] "cell"       "donor"      "experiment" "gene"      
```
