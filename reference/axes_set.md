# Names of all axes, sorted.

Names of all axes, sorted.

## Usage

``` r
axes_set(daf)
```

## Arguments

- daf:

  A `DafReader`.

## Value

Character vector of axis names.

## Examples

``` r
d <- example_cells_daf()
axes_set(d)
#> [1] "cell"       "donor"      "experiment" "gene"      
```
