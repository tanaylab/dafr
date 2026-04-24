# Lookup-matrix query operation.

Builds a `::` (optionally `:: <name>`) query fragment, looking up the
named matrix property indexed by the prior two axes.

## Usage

``` r
LookupMatrix(value = NULL, ...)
```

## Arguments

- value:

  Optional matrix name (character scalar), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`LookupScalar()`](https://tanaylab.github.io/dafr/reference/LookupScalar.md),
[`LookupVector()`](https://tanaylab.github.io/dafr/reference/LookupVector.md)

## Examples

``` r
LookupMatrix()
#> <DafrQuery> :: 
LookupMatrix("UMIs")
#> <DafrQuery> :: UMIs 
Axis("cell") |> Axis("gene") |> LookupMatrix("UMIs")
#> <DafrQuery> @ cell @ gene :: UMIs 
```
