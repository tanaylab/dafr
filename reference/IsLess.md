# IsLess query operation

A query operation for filtering based on numeric comparison. This
operation converts a vector property to a Boolean mask by comparing each
value to the specified threshold using the less-than (`<`) operator.
Only entries where the comparison returns true are included in the
result. Typically used after a Lookup operation to filter entries based
on numeric values. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IsLess)
for details.

## Usage

``` r
IsLess(value, ...)
```

## Arguments

- value:

  Threshold value to compare against

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
