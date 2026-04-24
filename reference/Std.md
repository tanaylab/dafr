# Reduction query operation: standard deviation.

Builds a `% Std` query fragment.

## Usage

``` r
Std(type = NULL, ...)
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

[`Var()`](https://tanaylab.github.io/dafr/reference/Var.md),
[`StdN()`](https://tanaylab.github.io/dafr/reference/StdN.md)

## Examples

``` r
Std()
#> <DafrQuery> % Std 
```
