# Escape a value for use in a query string

Escapes special characters in a value so it can be safely embedded in a
query string. This is needed when values contain characters that have
special meaning in the query syntax (e.g., spaces, quotes, backslashes).

## Usage

``` r
escape_value(value)
```

## Arguments

- value:

  A character string value to escape

## Value

The escaped value as a character string

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.escape_value)
for details.
