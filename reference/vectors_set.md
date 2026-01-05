# Get set of vector names for an axis in a Daf object

Returns the names of all vector properties for the specified axis.

## Usage

``` r
vectors_set(daf, axis)
```

## Arguments

- daf:

  A Daf object

- axis:

  Name of the axis

## Value

A character vector of vector property names

## Details

This function provides the complete set of available vector properties
for a specific axis that can be retrieved using
[`get_vector()`](https://tanaylab.github.io/dafr/reference/get_vector.md).
Vector properties store one-dimensional data along a specific axis. See
the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.vectors_set)
for details.
