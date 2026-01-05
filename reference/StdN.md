# StdN query operation

Reduction operation that returns the standard deviation of the values,
normalized (divided) by the mean of the values. This operation reduces
the dimensionality of the data: a matrix becomes a vector (normalized
standard deviation of each column), and a vector becomes a scalar
(normalized standard deviation of all elements). See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html#DataAxesFormats.Operations.Std)
for details.

## Usage

``` r
StdN(...)
```

## Arguments

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
