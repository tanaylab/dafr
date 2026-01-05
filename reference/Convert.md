# Convert query operation

Element-wise operation that converts every element to a given data type.
This operation preserves the shape of the data (scalar, vector, or
matrix) but changes the data type of the elements. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html#DataAxesFormats.Operations.Convert)
for details.

## Usage

``` r
Convert(type, ...)
```

## Arguments

- type:

  Type to convert to (e.g., "Int32", "Float64")

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
