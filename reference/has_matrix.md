# Test whether a matrix exists for an axis pair.

Test whether a matrix exists for an axis pair.

## Usage

``` r
has_matrix(daf, rows_axis, columns_axis, name, relayout = TRUE)
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

- relayout:

  If `TRUE` (default), also report `TRUE` when the matrix is stored only
  at the flipped axis pair `(columns_axis, rows_axis)`. Set to `FALSE`
  to ask the strict "this exact layout?" question. Mirrors Julia
  `has_matrix(...; relayout)`.

## Value

Logical scalar.

## Examples

``` r
# Mirrors readers.jl jldoctest at line 748.
has_matrix(example_cells_daf(), "gene", "cell", "UMIs") # TRUE
#> [1] TRUE

# `relayout = FALSE` asks the strict "this exact layout?" question:
has_matrix(example_cells_daf(), "gene", "cell", "UMIs", relayout = FALSE)
#> [1] TRUE
```
