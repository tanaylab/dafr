# Lookup query operation

A query operation for looking up the value of a property with a specific
name. This operation retrieves the data associated with the specified
property for the current axis context. It is typically used after an
`Axis` operation to select a property from that axis. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Lookup)
for details.

## Usage

``` r
Lookup(property, ...)
```

## Arguments

- property:

  String specifying the property name to look up

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
