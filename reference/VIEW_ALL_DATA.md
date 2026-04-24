# Default view spec: expose all data (scalars + vectors + matrices) as-is.

A convenience list combining
[VIEW_ALL_SCALARS](https://tanaylab.github.io/dafr/reference/VIEW_ALL_SCALARS.md),
[VIEW_ALL_VECTORS](https://tanaylab.github.io/dafr/reference/VIEW_ALL_VECTORS.md),
and
[VIEW_ALL_MATRICES](https://tanaylab.github.io/dafr/reference/VIEW_ALL_MATRICES.md).
Pass as the `data` argument to
[`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md) to
expose every data item from the base daf unchanged.

## Usage

``` r
VIEW_ALL_DATA
```

## Format

An object of class `list` of length 3.

## See also

[`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md),
[VIEW_ALL_SCALARS](https://tanaylab.github.io/dafr/reference/VIEW_ALL_SCALARS.md),
[VIEW_ALL_VECTORS](https://tanaylab.github.io/dafr/reference/VIEW_ALL_VECTORS.md),
[VIEW_ALL_MATRICES](https://tanaylab.github.io/dafr/reference/VIEW_ALL_MATRICES.md)

## Examples

``` r
d <- example_cells_daf()
v <- viewer(d, data = VIEW_ALL_DATA)
scalars_set(v)
#> [1] "organism"  "reference"
```
