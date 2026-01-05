# Names query operation

A query operation for looking up a set of names in a Daf object. This
operation retrieves metadata names such as axis names, property names,
or scalar names. It is often used to discover what data is available in
a Daf object. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Names)
for details.

## Usage

``` r
Names(kind = NULL, ...)
```

## Arguments

- kind:

  Optional string specifying the kind of names to look up (e.g., "axes",
  "scalars")

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
