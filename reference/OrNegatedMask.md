# Or-negated-mask query operation.

Builds a `| ! <property>` query fragment that chains a logical OR NOT
condition into an open mask subquery.

## Usage

``` r
OrNegatedMask(value, ...)
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

[`OrMask()`](https://tanaylab.github.io/dafr/reference/OrMask.md),
[`AndNegatedMask()`](https://tanaylab.github.io/dafr/reference/AndNegatedMask.md),
[`XorNegatedMask()`](https://tanaylab.github.io/dafr/reference/XorNegatedMask.md),
[`BeginMask()`](https://tanaylab.github.io/dafr/reference/BeginMask.md),
[`EndMask()`](https://tanaylab.github.io/dafr/reference/EndMask.md)

## Examples

``` r
OrNegatedMask("type")
#> <DafrQuery> | ! type 
BeginMask("a") |> OrNegatedMask("b") |> EndMask()
#> <DafrQuery> [ a | ! b ] 
```
