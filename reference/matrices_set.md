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
# Mirrors readers.jl jldoctest at line 801.
matrices_set(example_cells_daf(), "gene", "cell") # "UMIs"
#> [1] "UMIs"
```
