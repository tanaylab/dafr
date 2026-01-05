# Axis query operation

A query operation for specifying a result axis in a query sequence. This
is typically the first operation in a query sequence and determines
which axis the query will operate on. It sets the context for subsequent
operations in the query. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Axis)
for details.

## Usage

``` r
Axis(axis, ...)
```

## Arguments

- axis:

  String specifying the axis name

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
