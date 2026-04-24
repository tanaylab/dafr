# Names of matrices for an axis pair, sorted.

Names of matrices for an axis pair, sorted.

## Usage

``` r
matrices_set(daf, rows_axis, columns_axis)
```

## Arguments

- daf:

  A `DafReader`.

- rows_axis:

  Row-axis name.

- columns_axis:

  Column-axis name.

## Value

Character vector.

## Examples

``` r
d <- example_cells_daf()
matrices_set(d, "gene", "cell")
#> [1] "UMIs"
```
