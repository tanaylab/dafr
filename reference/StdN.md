# Reduction query operation: standard deviation normalised by the mean.

Builds a `% StdN` query fragment (optionally with `eps` to avoid
division by zero).

## Usage

``` r
StdN(type = NULL, ...)
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

[`Std()`](https://tanaylab.github.io/dafr/reference/Std.md)

## Examples

``` r
StdN()
#> <DafrQuery> % StdN 
StdN(eps = 1)
#> <DafrQuery> % StdN eps: 1 
```
