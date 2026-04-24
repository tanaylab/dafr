# Axis selection query operation.

Builds an `@ <axis_name>` query fragment, selecting the named axis as
the next input to the query.

## Usage

``` r
Axis(value, ...)
```

## Arguments

- value:

  Axis name (character scalar), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`AsAxis()`](https://tanaylab.github.io/dafr/reference/AsAxis.md),
[`LookupVector()`](https://tanaylab.github.io/dafr/reference/LookupVector.md),
[`LookupMatrix()`](https://tanaylab.github.io/dafr/reference/LookupMatrix.md)

## Examples

``` r
Axis("cell")
#> <DafrQuery> @ cell 
Axis("cell") |> Axis("gene")
#> <DafrQuery> @ cell @ gene 
```
