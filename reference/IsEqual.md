# IsEqual query operation

Equality is used for two purposes: As a comparison operator, similar to
`IsLess` except that uses `=` instead of `<` for the comparison; and To
select a single entry from a vector. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IsEqual)
for details.

## Usage

``` r
IsEqual(value, ...)
```

## Arguments

- value:

  Value to compare against

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object
