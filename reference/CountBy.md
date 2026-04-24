# Count-by query operation.

Builds a `* <property>` query fragment. Counts entries of the prior
axis, grouped by `property`.

## Usage

``` r
CountBy(value, ...)
```

## Arguments

- value:

  Property name to count by, or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`GroupBy()`](https://tanaylab.github.io/dafr/reference/GroupBy.md)

## Examples

``` r
CountBy("donor")
#> <DafrQuery> * donor 
```
