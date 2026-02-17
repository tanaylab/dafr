# Log query operation

Element-wise operation that converts every element to its logarithm.
This operation preserves the shape of the data (scalar, vector, or
matrix) but changes each value to its logarithm with the specified base.
See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html#DataAxesFormats.Operations.Log)
for details.

## Usage

``` r
Log(base = exp(1), eps = 0, type = NULL, ...)
```

## Arguments

- base:

  Base of the logarithm (default is e ≈ 2.718)

- eps:

  Small value added to avoid log(0) (default is 0)

- type:

  Optional result type (e.g., "Float64"). If NULL, the default type is
  used.

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
