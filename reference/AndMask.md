# And-mask query operation.

Builds a `& <property>` query fragment that chains a logical AND
condition into an open mask subquery. Use after
[`BeginMask()`](https://tanaylab.github.io/dafr/reference/BeginMask.md)
or another mask combinator; close the mask with
[`EndMask()`](https://tanaylab.github.io/dafr/reference/EndMask.md).

## Usage

``` r
AndMask(value, ...)
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

[`AndNegatedMask()`](https://tanaylab.github.io/dafr/reference/AndNegatedMask.md),
[`OrMask()`](https://tanaylab.github.io/dafr/reference/OrMask.md),
[`XorMask()`](https://tanaylab.github.io/dafr/reference/XorMask.md),
[`BeginMask()`](https://tanaylab.github.io/dafr/reference/BeginMask.md),
[`EndMask()`](https://tanaylab.github.io/dafr/reference/EndMask.md)

## Examples

``` r
AndMask("type")
#> <DafrQuery> & type 
BeginMask("a") |> AndMask("b") |> EndMask()
#> <DafrQuery> [ a & b ] 
```
