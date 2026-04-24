# Reduction query operation: count non-zero entries.

Builds a `% Count` query fragment.

## Usage

``` r
Count(type = NULL, ...)
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

[`Sum()`](https://tanaylab.github.io/dafr/reference/Sum.md)

## Examples

``` r
Count()
#> <DafrQuery> % Count 
```
