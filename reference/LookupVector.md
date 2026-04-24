# Lookup-vector query operation.

Builds a `:` (optionally `: <name>`) query fragment, looking up the
named vector property of the prior axis.

## Usage

``` r
LookupVector(value = NULL, ...)
```

## Arguments

- value:

  Optional vector name (character scalar), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`LookupScalar()`](https://tanaylab.github.io/dafr/reference/LookupScalar.md),
[`LookupMatrix()`](https://tanaylab.github.io/dafr/reference/LookupMatrix.md)

## Examples

``` r
LookupVector()
#> <DafrQuery> : 
LookupVector("age")
#> <DafrQuery> : age 
Axis("cell") |> LookupVector("age")
#> <DafrQuery> @ cell : age 
```
