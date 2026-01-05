# Max query operation

Reduction operation that returns the maximal element. This operation
reduces the dimensionality of the data: a matrix becomes a vector
(maximum of each column), and a vector becomes a scalar (maximum of all
elements). See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html#DataAxesFormats.Operations.Max)
for details.

## Usage

``` r
Max(...)
```

## Arguments

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
