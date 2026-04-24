# And-negated-mask query operation.

Builds a `& ! <property>` query fragment that chains a logical AND NOT
condition into an open mask subquery.

## Usage

``` r
AndNegatedMask(value, ...)
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

[`AndMask()`](https://tanaylab.github.io/dafr/reference/AndMask.md),
[`OrNegatedMask()`](https://tanaylab.github.io/dafr/reference/OrNegatedMask.md),
[`XorNegatedMask()`](https://tanaylab.github.io/dafr/reference/XorNegatedMask.md),
[`BeginMask()`](https://tanaylab.github.io/dafr/reference/BeginMask.md),
[`EndMask()`](https://tanaylab.github.io/dafr/reference/EndMask.md)

## Examples

``` r
AndNegatedMask("type")
#> <DafrQuery> & ! type 
BeginMask("a") |> AndNegatedMask("b") |> EndMask()
#> <DafrQuery> [ a & ! b ] 
```
