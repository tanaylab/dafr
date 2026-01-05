# And query operation

A query operation for filtering axis entries using a Boolean mask. This
operation restricts the set of entries of an axis to only those where
the specified property contains true values (or non-zero/non-empty
values). It essentially performs a logical AND between the current
selection and the specified property, treating the property as a Boolean
mask. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.And)
for details.

## Usage

``` r
And(property, ...)
```

## Arguments

- property:

  String specifying the property to use as a filter mask

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
