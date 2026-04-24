# Negated begin-mask query operation.

Builds a `[ ! <property>` query fragment that opens a negated masked
subquery against `property`. Close the mask with
[`EndMask()`](https://tanaylab.github.io/dafr/reference/EndMask.md).

## Usage

``` r
BeginNegatedMask(value, ...)
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

[`BeginMask()`](https://tanaylab.github.io/dafr/reference/BeginMask.md),
[`EndMask()`](https://tanaylab.github.io/dafr/reference/EndMask.md)

## Examples

``` r
BeginNegatedMask("type")
#> <DafrQuery> [ ! type 
```
