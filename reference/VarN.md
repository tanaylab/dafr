# Reduction query operation: variance normalised by the mean.

Builds a `% VarN` query fragment (optionally with `eps` to avoid
division by zero).

## Usage

``` r
VarN(type = NULL, ...)
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

[`Var()`](https://tanaylab.github.io/dafr/reference/Var.md)

## Examples

``` r
VarN()
#> <DafrQuery> % VarN 
VarN(eps = 1)
#> <DafrQuery> % VarN eps: 1 
```
