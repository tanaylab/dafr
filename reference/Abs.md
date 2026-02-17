# Abs query operation

Element-wise operation that converts every element to its absolute
value. This operation preserves the shape of the data (scalar, vector,
or matrix) but changes the values. By default, the output data type is
the unsigned version of the input data type. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html#DataAxesFormats.Operations.Abs)
for details.

## Usage

``` r
Abs(type = NULL, ...)
```

## Arguments

- type:

  Optional result type (e.g., "Float64"). If NULL, the default type is
  used.

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
