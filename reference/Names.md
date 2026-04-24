# Names query operation.

Builds a `?` query fragment, listing the entries of the prior axis.

## Usage

``` r
Names(...)
```

## Arguments

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`Axis()`](https://tanaylab.github.io/dafr/reference/Axis.md)

## Examples

``` r
Names()
#> <DafrQuery> ? 
Axis("cell") |> Names()
#> <DafrQuery> @ cell ? 
```
