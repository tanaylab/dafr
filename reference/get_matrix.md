# Get matrix from a Daf object

Retrieves a matrix property with the specified name for the given axes
from the Daf data set.

## Usage

``` r
get_matrix(daf, rows_axis, columns_axis, name, default = NULL, relayout = TRUE)
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

- default:

  Default value if matrix doesn't exist (NULL by default)

- relayout:

  Whether to allow retrieving matrix with flipped axes (TRUE by default)

## Value

A matrix with row and column names set to the axis entry names, or the
default value if the property doesn't exist

## Details

Matrix properties store two-dimensional data along two axes. If the
matrix doesn't exist and default is NA, a matrix of NAs with appropriate
dimensions is returned. If `relayout` is TRUE and the matrix exists with
flipped axes, it will be transposed automatically. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.get_matrix)
for details.
