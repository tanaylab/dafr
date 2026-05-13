# Wildcard for all scalars.

Wildcard for all scalars.

## Usage

``` r
ALL_SCALARS
```

## Examples

``` r
d <- memory_daf()
set_scalar(d, "organism", "human")
v <- viewer(d, data = list(list(ALL_SCALARS, "=")))
scalars_set(v)
#> [1] "organism"
```
