# Clamp query operation

Element-wise operation that converts every element to a value inside a
range. This operation preserves the shape of the data (scalar, vector,
or matrix) but changes the values, setting each value to be within the
specified range. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html#DataAxesFormats.Operations.Clamp)
for details.

## Usage

``` r
Clamp(min = NULL, max = NULL, ...)
```

## Arguments

- min:

  Minimum value; values less than this will be set to this value

- max:

  Maximum value; values greater than this will be set to this value

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
