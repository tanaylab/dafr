# Wildcard for all axes.

Wildcard for all axes.

## Usage

``` r
ALL_AXES
```

## Format

An object of class `character` of length 1.

## Examples

``` r
d <- example_cells_daf()
v <- viewer(d, axes = list(list(ALL_AXES, "=")))
axes_set(v)
#> [1] "cell"       "donor"      "experiment" "gene"      
```
