# Element-wise query operation: logarithm.

Builds a `% Log <params>` query fragment.

## Usage

``` r
Log(type = NULL, ...)
```

## Arguments

- type:

  Optional output type (character scalar) or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Named parameters `base` (default natural log), `eps` (added to
  operands to avoid `log(0)`); or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`Fraction()`](https://tanaylab.github.io/dafr/reference/Fraction.md),
[`Clamp()`](https://tanaylab.github.io/dafr/reference/Clamp.md)

## Examples

``` r
Log(base = 2, eps = 1e-6)
#> <DafrQuery> % Log base: 2 eps: 1e-06 
```
