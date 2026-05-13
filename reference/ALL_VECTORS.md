# Wildcard for all vectors.

Wildcard for all vectors.

## Usage

``` r
ALL_VECTORS
```

## Examples

``` r
d <- example_cells_daf()
v <- viewer(d, data = list(list(ALL_VECTORS, "=")))
vectors_set(v, "cell")
#> [1] "donor"      "experiment"
```
