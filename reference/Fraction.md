# Element-wise query operation: convert each entry to its fraction of the total.

Builds a `% Fraction` query fragment. Each value is divided by the sum
of its vector (or matrix column).

## Usage

``` r
Fraction(type = NULL, ...)
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

[`Log()`](https://tanaylab.github.io/dafr/reference/Log.md),
[`Clamp()`](https://tanaylab.github.io/dafr/reference/Clamp.md)

## Examples

``` r
Fraction()
#> <DafrQuery> % Fraction 
```
