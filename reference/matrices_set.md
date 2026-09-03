# Names of matrices for an axis pair, sorted.

Names of matrices for an axis pair, sorted.

## Usage

``` r
matrices_set(daf, rows_axis, columns_axis, relayout = TRUE)
```

## Arguments

- daf:

  A `DafReader`.

- rows_axis:

  Row-axis name.

- columns_axis:

  Column-axis name.

- relayout:

  If `TRUE` (the default, matching Julia
  `matrices_set(...; relayout = true)`), also include the names of
  matrices stored only in the flipped layout. If `FALSE`, this lists
  exactly the matrices stored in this layout, which is exactly the set
  [`get_matrix()`](https://tanaylab.github.io/dafr/reference/get_matrix.md)
  will give when asked the same way.

## Value

Character vector.

## Examples

``` r
# Mirrors readers.jl jldoctest at line 801.
matrices_set(example_cells_daf(), "gene", "cell") # "UMIs"
#> [1] "UMIs"
```
