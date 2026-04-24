# Or-mask query operation.

Builds a `| <property>` query fragment that chains a logical OR
condition into an open mask subquery.

## Usage

``` r
OrMask(value, ...)
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

[`OrNegatedMask()`](https://tanaylab.github.io/dafr/reference/OrNegatedMask.md),
[`AndMask()`](https://tanaylab.github.io/dafr/reference/AndMask.md),
[`XorMask()`](https://tanaylab.github.io/dafr/reference/XorMask.md),
[`BeginMask()`](https://tanaylab.github.io/dafr/reference/BeginMask.md),
[`EndMask()`](https://tanaylab.github.io/dafr/reference/EndMask.md)

## Examples

``` r
OrMask("type")
#> <DafrQuery> | type 
BeginMask("a") |> OrMask("b") |> EndMask()
#> <DafrQuery> [ a | b ] 
```
