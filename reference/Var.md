# Reduction query operation: variance.

Builds a `% Var` query fragment.

## Usage

``` r
Var(type = NULL, ...)
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

[`Std()`](https://tanaylab.github.io/dafr/reference/Std.md),
[`VarN()`](https://tanaylab.github.io/dafr/reference/VarN.md)

## Examples

``` r
Var()
#> <DafrQuery> % Var 
```
