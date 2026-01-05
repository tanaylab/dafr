# SquareMaskColumn query operation

Similar to `MaskSlice` but is used when the mask matrix is square and
we'd like to use a column as a mask. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.SquareMaskColumn)
for details.

## Usage

``` r
SquareMaskColumn(value, ...)
```

## Arguments

- value:

  String specifying the value

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object
