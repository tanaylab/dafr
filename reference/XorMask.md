# Xor-mask query operation.

Builds a `^ <property>` query fragment that chains a logical XOR
condition into an open mask subquery.

## Usage

``` r
XorMask(value, ...)
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

[`XorNegatedMask()`](https://tanaylab.github.io/dafr/reference/XorNegatedMask.md),
[`AndMask()`](https://tanaylab.github.io/dafr/reference/AndMask.md),
[`OrMask()`](https://tanaylab.github.io/dafr/reference/OrMask.md),
[`BeginMask()`](https://tanaylab.github.io/dafr/reference/BeginMask.md),
[`EndMask()`](https://tanaylab.github.io/dafr/reference/EndMask.md)

## Examples

``` r
XorMask("type")
#> <DafrQuery> ^ type 
BeginMask("a") |> XorMask("b") |> EndMask()
#> <DafrQuery> [ a ^ b ] 
```
