# End-mask query operation.

Builds a `]` query fragment that closes a masked subquery opened with
[`BeginMask()`](https://tanaylab.github.io/dafr/reference/BeginMask.md)
or
[`BeginNegatedMask()`](https://tanaylab.github.io/dafr/reference/BeginNegatedMask.md).

## Usage

``` r
EndMask(...)
```

## Arguments

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`BeginMask()`](https://tanaylab.github.io/dafr/reference/BeginMask.md),
[`BeginNegatedMask()`](https://tanaylab.github.io/dafr/reference/BeginNegatedMask.md)

## Examples

``` r
EndMask()
#> <DafrQuery> ] 
Axis("cell") |> BeginMask("type") |> EndMask()
#> <DafrQuery> @ cell [ type ] 
```
