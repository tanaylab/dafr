# Round query operation

Element-wise operation that converts every element to the nearest
integer value. This operation preserves the shape of the data (scalar,
vector, or matrix) but rounds each value to the nearest integer. See the
Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html#DataAxesFormats.Operations.Round)
for details.

## Usage

``` r
Round(...)
```

## Arguments

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
