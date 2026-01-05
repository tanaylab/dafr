# Check if a matrix exists in a Daf object

Determines whether a matrix property with the specified name exists for
the given axes.

## Usage

``` r
has_matrix(daf, rows_axis, columns_axis, name, relayout = TRUE)
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

- relayout:

  Whether to check with flipped axes too (TRUE by default)

## Value

TRUE if matrix exists, FALSE otherwise

## Details

Matrix properties store two-dimensional data along two axes. If
`relayout` is TRUE, this function will also check if the matrix exists
with the axes flipped (i.e., rows as columns and columns as rows). See
the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.has_matrix)
for details.
