# Inverse of [`escape_value()`](https://tanaylab.github.io/dafr/reference/escape_value.md).

Strips an outer pair of double quotes (if present) and unescapes `\\`
and `\"` sequences. Leaves already-bare strings unchanged.

## Usage

``` r
unescape_value(s)
```

## Arguments

- s:

  Character scalar (an escaped query literal).

## Value

Character scalar (the original value).

## See also

[`escape_value()`](https://tanaylab.github.io/dafr/reference/escape_value.md)

## Examples

``` r
unescape_value("\"a b\"")
#> [1] "a b"
unescape_value("plain")
#> [1] "plain"
stopifnot(identical(unescape_value(escape_value("a b")), "a b"))
```
