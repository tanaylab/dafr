# Group-by query operation.

Builds a `/ <property>` query fragment. Partitions the prior axis's
entries by `property` and then expects a subsequent reduction
([`Sum()`](https://tanaylab.github.io/dafr/reference/Sum.md),
[`Mean()`](https://tanaylab.github.io/dafr/reference/Mean.md), ...).

## Usage

``` r
GroupBy(value, ...)
```

## Arguments

- value:

  Property name to group by (character scalar), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`GroupRowsBy()`](https://tanaylab.github.io/dafr/reference/GroupRowsBy.md),
[`GroupColumnsBy()`](https://tanaylab.github.io/dafr/reference/GroupColumnsBy.md),
[`CountBy()`](https://tanaylab.github.io/dafr/reference/CountBy.md)

## Examples

``` r
GroupBy("donor")
#> <DafrQuery> / donor 
```
