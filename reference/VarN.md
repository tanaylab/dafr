# VarN query operation

Reduction operation that returns the variance of the values, normalized
(divided) by the mean of the values. This operation reduces the
dimensionality of the data: a matrix becomes a vector (normalized
variance of each column), and a vector becomes a scalar (normalized
variance of all elements). See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html#DataAxesFormats.Operations.Var)
for details.

## Usage

``` r
VarN(...)
```

## Arguments

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
