# Wildcard for all axes.

Wildcard for all axes.

## Usage

``` r
ALL_AXES
```

## Examples

``` r
d <- example_cells_daf()
v <- viewer(d, axes = list(list(ALL_AXES, "=")))
axes_set(v)
#> [1] "cell"       "donor"      "experiment" "gene"      
```
