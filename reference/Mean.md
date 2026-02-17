# Mean query operation

Reduction operation that returns the mean value. This operation reduces
the dimensionality of the data: a matrix becomes a vector (mean of each
column), and a vector becomes a scalar (mean of all elements). See the
Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html#DataAxesFormats.Operations.Mean)
for details.

## Usage

``` r
Mean(type = NULL, ...)
```

## Arguments

- type:

  Optional result type (e.g., "Float64"). If NULL, the default type is
  used.

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
