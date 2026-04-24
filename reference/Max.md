# Reduction query operation: maximum.

Builds a `% Max` query fragment. Consumes a matrix axis (or the single
axis of a vector) and produces a reduced result.

## Usage

``` r
Max(...)
```

## Arguments

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`Min()`](https://tanaylab.github.io/dafr/reference/Min.md),
[`Mean()`](https://tanaylab.github.io/dafr/reference/Mean.md),
[`Sum()`](https://tanaylab.github.io/dafr/reference/Sum.md)

## Examples

``` r
Max()
#> <DafrQuery> % Max 
```
