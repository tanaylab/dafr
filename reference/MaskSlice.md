# MaskSlice query operation

A query operation for using a slice of a matrix as a mask, when the
other axis of the matrix is different from the mask axis. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.MaskSlice)
for details.

## Usage

``` r
MaskSlice(axis, ...)
```

## Arguments

- axis:

  String specifying the axis

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object
