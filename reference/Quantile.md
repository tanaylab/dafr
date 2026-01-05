# Quantile query operation

Reduction operation that returns the quantile value, that is, a value
such that a certain fraction of the values is lower. This operation
reduces the dimensionality of the data: a matrix becomes a vector
(quantile of each column), and a vector becomes a scalar (quantile of
all elements). See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html#DataAxesFormats.Operations.Quantile)
for details.

## Usage

``` r
Quantile(p = 0.5, ...)
```

## Arguments

- p:

  Quantile to compute (between 0 and 1). Default is 0.5 (median).

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
