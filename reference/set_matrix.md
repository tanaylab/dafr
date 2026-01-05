# Set matrix in a Daf object

Sets a matrix property with the specified name for the given axes in the
Daf data set.

## Usage

``` r
set_matrix(
  daf,
  rows_axis,
  columns_axis,
  name,
  value,
  overwrite = FALSE,
  relayout = TRUE
)
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

- value:

  Matrix of values to set (cannot contain NA values)

- overwrite:

  Whether to overwrite if matrix already exists (FALSE by default)

- relayout:

  Whether to allow relayout with flipped axes (TRUE by default)

## Value

The Daf object (invisibly, for chaining operations)

## Details

This function creates or updates a matrix property in the Daf data set.
The dimensions of the matrix must match the lengths of the specified
axes. If the matrix already exists and `overwrite` is FALSE, an error
will be raised. If `relayout` is TRUE, the matrix will also be stored
with axes flipped for faster access. NA values are not supported in Daf.
See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.set_matrix!)
for details.
