# Begin-mask query operation.

Builds a `[ <property>` query fragment that opens a masked subquery
against `property`. Close the mask with
[`EndMask()`](https://tanaylab.github.io/dafr/reference/EndMask.md).

## Usage

``` r
BeginMask(value, ...)
```

## Arguments

- value:

  Property name (character scalar), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`BeginNegatedMask()`](https://tanaylab.github.io/dafr/reference/BeginNegatedMask.md),
[`EndMask()`](https://tanaylab.github.io/dafr/reference/EndMask.md),
[`AndMask()`](https://tanaylab.github.io/dafr/reference/AndMask.md)

## Examples

``` r
BeginMask("type")
#> <DafrQuery> [ type 
Axis("cell") |> BeginMask("type") |> EndMask()
#> <DafrQuery> @ cell [ type ] 
```
