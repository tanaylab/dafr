# Relayout matrix in a Daf object

Creates or updates a matrix property with flipped axes for more
efficient access.

## Usage

``` r
relayout_matrix(daf, rows_axis, columns_axis, name, overwrite = FALSE)
```

## Arguments

- daf:

  A Daf object

- rows_axis:

  Name of rows axis

- columns_axis:

  Name of columns axis

- name:

  Name of the matrix property

- overwrite:

  Whether to overwrite if matrix already exists with flipped axes (FALSE
  by default)

## Value

The Daf object (invisibly, for chaining operations)

## Details

This function creates a transposed version of an existing matrix
property, allowing efficient access from either axis orientation. If a
matrix with the flipped axes already exists and `overwrite` is FALSE, an
error will be raised. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.relayout_matrix!)
for details.
