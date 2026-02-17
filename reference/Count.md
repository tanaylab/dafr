# Count query operation

Reduction operation that counts the non-zero elements. This operation
reduces the dimensionality of the data: a matrix becomes a vector (count
of non-zero elements in each column), and a vector becomes a scalar
(count of non-zero elements). See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html#DataAxesFormats.Operations.Count)
for details.

## Usage

``` r
Count(type = NULL, ...)
```

## Arguments

- type:

  Optional result type (e.g., "Float64"). If NULL, the default type is
  used.

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
