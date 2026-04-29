# Delete a matrix.

Delete a matrix.

## Usage

``` r
delete_matrix(
  daf,
  rows_axis,
  columns_axis,
  name,
  must_exist = TRUE,
  relayout = TRUE
)
```

## Arguments

- daf:

  A `DafReader`.

- rows_axis:

  Row-axis name.

- columns_axis:

  Column-axis name.

- name:

  Matrix name.

- must_exist:

  See `delete_axis`.

- relayout:

  If `TRUE` (default), also delete the flipped layout
  `(columns_axis, rows_axis, name)` if present. Mirrors Julia
  `delete_matrix!(...; relayout)`.

## Value

Invisibly the input `daf`.

## Examples

``` r
# Mirrors writers.jl jldoctest at line 1147.
d <- example_cells_daf()
has_matrix(d, "gene", "cell", "UMIs", relayout = FALSE)  # TRUE
#> [1] TRUE
has_matrix(d, "cell", "gene", "UMIs", relayout = FALSE)  # TRUE
#> [1] TRUE
delete_matrix(d, "gene", "cell", "UMIs", relayout = FALSE)
has_matrix(d, "gene", "cell", "UMIs", relayout = FALSE)  # FALSE
#> [1] FALSE
has_matrix(d, "cell", "gene", "UMIs", relayout = FALSE)  # TRUE
#> [1] TRUE
delete_matrix(d, "gene", "cell", "UMIs", must_exist = FALSE)
has_matrix(d, "gene", "cell", "UMIs", relayout = FALSE)  # FALSE
#> [1] FALSE
has_matrix(d, "cell", "gene", "UMIs", relayout = FALSE)  # FALSE
#> [1] FALSE
```
