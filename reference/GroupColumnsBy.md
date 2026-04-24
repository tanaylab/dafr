# Group-columns-by query operation.

Builds a `|/ <property>` query fragment. Groups columns of a matrix by
`property`; typically followed by
[`ReduceToColumn()`](https://tanaylab.github.io/dafr/reference/ReduceToColumn.md).

## Usage

``` r
GroupColumnsBy(value, ...)
```

## Arguments

- value:

  Property name to group columns by, or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`GroupBy()`](https://tanaylab.github.io/dafr/reference/GroupBy.md),
[`GroupRowsBy()`](https://tanaylab.github.io/dafr/reference/GroupRowsBy.md)

## Examples

``` r
GroupColumnsBy("type")
#> <DafrQuery> |/ type 
```
