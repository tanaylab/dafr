# Reduction query operation: geometric mean.

Builds a `% GeoMean` query fragment (optionally with `eps` to avoid
`log(0)`).

## Usage

``` r
GeoMean(type = NULL, ...)
```

## Arguments

- type:

  Optional output type (character scalar) or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Named parameter `eps`; or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`Mean()`](https://tanaylab.github.io/dafr/reference/Mean.md)

## Examples

``` r
GeoMean()
#> <DafrQuery> % GeoMean 
GeoMean(eps = 1)
#> <DafrQuery> % GeoMean eps: 1 
```
