# IsNotMatch query operation

Similar to `IsMatch` except that looks for entries that do not match the
pattern. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IsNotMatch)
for details.

## Usage

``` r
IsNotMatch(value, ...)
```

## Arguments

- value:

  Regular expression pattern to not match against

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object
