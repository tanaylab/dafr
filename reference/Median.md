# Reduction query operation: median.

Builds a `% Median` query fragment.

## Usage

``` r
Median(type = NULL, ...)
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

[`Mean()`](https://tanaylab.github.io/dafr/reference/Mean.md),
[`Quantile()`](https://tanaylab.github.io/dafr/reference/Quantile.md)

## Examples

``` r
Median()
#> <DafrQuery> % Median 
```
