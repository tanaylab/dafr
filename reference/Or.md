# Or query operation

A query operation for expanding the set of entries of an axis. This
operation adds entries to the current selection where the specified
property contains true values (or non-zero/non-empty values). It
performs a logical OR between the current selection and the specified
property, treating the property as a Boolean mask. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Or)
for details.

## Usage

``` r
Or(property, ...)
```

## Arguments

- property:

  String specifying the property to use for expanding the selection

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
