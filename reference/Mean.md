# Reduction query operation: mean.

Builds a `% Mean` query fragment (optionally `% Mean type: <T>`).

## Usage

``` r
Mean(type = NULL, ...)
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

[`Sum()`](https://tanaylab.github.io/dafr/reference/Sum.md),
[`Median()`](https://tanaylab.github.io/dafr/reference/Median.md)

## Examples

``` r
Mean()
#> <DafrQuery> % Mean 
Mean(type = "Float64")
#> <DafrQuery> % Mean type: Float64 
```
