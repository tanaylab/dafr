# Group-rows-by query operation.

Builds a `-/ <property>` query fragment. Groups rows of a matrix by
`property`; typically followed by
[`ReduceToRow()`](https://tanaylab.github.io/dafr/reference/ReduceToRow.md).

## Usage

``` r
GroupRowsBy(value, ...)
```

## Arguments

- value:

  Property name to group rows by, or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`GroupBy()`](https://tanaylab.github.io/dafr/reference/GroupBy.md),
[`GroupColumnsBy()`](https://tanaylab.github.io/dafr/reference/GroupColumnsBy.md)

## Examples

``` r
GroupRowsBy("donor")
#> <DafrQuery> -/ donor 
```
