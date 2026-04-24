# Default view spec: expose all scalars as-is.

A pre-built `data` item for
[`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md) that
exposes every scalar of the base daf unchanged. Equivalent to
`list(list(ALL_SCALARS, "="))`.

## Usage

``` r
VIEW_ALL_SCALARS
```

## Format

An object of class `list` of length 1.

## See also

[`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md),
[ALL_SCALARS](https://tanaylab.github.io/dafr/reference/ALL_SCALARS.md)

## Examples

``` r
d <- memory_daf()
set_scalar(d, "organism", "human")
v <- viewer(d, data = list(VIEW_ALL_SCALARS))
scalars_set(v)
#> [1] "organism"
```
