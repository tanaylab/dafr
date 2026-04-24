# Escape a value for use as a query literal.

If `s` contains any of the query metacharacters (whitespace, `!`, `&`,
`*`, `%`, `.`, `/`, `:`, `<`, `=`, `>`, `?`, `@`, `[`, `]`, `^`, `|`,
`~`, `"`), the result is double-quoted and any backslash or double-quote
inside is backslash-escaped. Otherwise `s` is returned unchanged.

## Usage

``` r
escape_value(s)
```

## Arguments

- s:

  Character scalar.

## Value

Character scalar suitable for concatenation into a query string.

## See also

[`unescape_value()`](https://tanaylab.github.io/dafr/reference/unescape_value.md),
[`canonical_query()`](https://tanaylab.github.io/dafr/reference/canonical_query.md)

## Examples

``` r
escape_value("plain")
#> [1] "plain"
escape_value("has space")
#> [1] "\"has space\""
unescape_value(escape_value("has \"quotes\""))
#> [1] "has \"quotes\""
```
