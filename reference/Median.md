# Median query operation

Reduction operation that returns the median value. This operation
reduces the dimensionality of the data: a matrix becomes a vector
(median of each column), and a vector becomes a scalar (median of all
elements). See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html#DataAxesFormats.Operations.Median)
for details.

## Usage

``` r
Median(...)
```

## Arguments

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
