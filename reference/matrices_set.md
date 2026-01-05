# Get set of matrix names for axes in a Daf object

Returns the names of all matrix properties for the specified pair of
axes.

## Usage

``` r
matrices_set(daf, rows_axis, columns_axis, relayout = TRUE)
```

## Arguments

- daf:

  A Daf object

- rows_axis:

  Name of rows axis

- columns_axis:

  Name of columns axis

- relayout:

  Whether to include matrices with flipped axes (TRUE by default)

## Value

A character vector of matrix property names

## Details

This function provides the complete set of available matrix properties
for specific axes that can be retrieved using
[`get_matrix()`](https://tanaylab.github.io/dafr/reference/get_matrix.md).
If `relayout` is TRUE, matrices stored with the axes flipped are also
included. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.matrices_set)
for details.
