# Mode query operation

Reduction operation that returns the most common value. This operation
reduces the dimensionality of the data: a matrix becomes a vector (mode
of each column), and a vector becomes a scalar (mode of all elements).
See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html#DataAxesFormats.Operations.Mode)
for details.

## Usage

``` r
Mode(...)
```

## Arguments

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
