# Names of vectors on an axis, sorted.

Names of vectors on an axis, sorted.

## Usage

``` r
vectors_set(daf, axis)
```

## Arguments

- daf:

  A `DafReader`.

- axis:

  Axis name.

## Value

Character vector.

## Examples

``` r
d <- example_cells_daf()
vectors_set(d, "cell")
#> [1] "donor"      "experiment"
```
