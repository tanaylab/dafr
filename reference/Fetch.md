# Fetch query operation

A query operation for fetching the value of a property from another
axis. This operation is typically used after a `Lookup` operation that
returns a vector whose values are entry names of another axis. The
`Fetch` operation will then retrieve a property from that other axis
based on these entry names. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Fetch)
for details.

## Usage

``` r
Fetch(property, ...)
```

## Arguments

- property:

  String specifying the property name to fetch from the target axis

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
