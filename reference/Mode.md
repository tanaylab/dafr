# Reduction query operation: most-frequent value.

Builds a `% Mode` query fragment. Returns the most common value of the
reduced axis. Numeric-only.

## Usage

``` r
Mode(type = NULL, ...)
```

## Arguments

- type:

  Optional output type (character scalar) or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`Median()`](https://tanaylab.github.io/dafr/reference/Median.md)

## Examples

``` r
Mode()
#> <DafrQuery> % Mode 
```
