# IsMatch query operation

Similar to `IsLess` except that the compared values must be strings, and
the mask is of the values that match the given regular expression. See
the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IsMatch)
for details.

## Usage

``` r
IsMatch(value, ...)
```

## Arguments

- value:

  Regular expression pattern to match against

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object
