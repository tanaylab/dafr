# IsGreaterEqual query operation

Similar to `IsLess` except that uses `>=` instead of `<` for the
comparison. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IsGreaterEqual)
for details.

## Usage

``` r
IsGreaterEqual(value, ...)
```

## Arguments

- value:

  Value to compare against

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object
