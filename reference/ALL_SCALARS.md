# Wildcard for all scalars.

Wildcard for all scalars.

## Usage

``` r
ALL_SCALARS
```

## Format

An object of class `character` of length 1.

## Examples

``` r
d <- memory_daf()
set_scalar(d, "organism", "human")
v <- viewer(d, data = list(list(ALL_SCALARS, "=")))
scalars_set(v)
#> [1] "organism"
```
