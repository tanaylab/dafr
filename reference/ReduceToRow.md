# Reduce-to-row query operation.

Builds a `>- <Reduction>` query fragment from a reduction query.
Converts each column of a matrix to a single value via the given
reduction.

## Usage

``` r
ReduceToRow(reduction, ...)
```

## Arguments

- reduction:

  A reduction
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md)
  (e.g. [`Sum()`](https://tanaylab.github.io/dafr/reference/Sum.md),
  [`Mean()`](https://tanaylab.github.io/dafr/reference/Mean.md)), or a
  piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md)
  with the reduction supplied in `...`.

- ...:

  The reduction
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md)
  when `reduction` holds the piped prior.

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`ReduceToColumn()`](https://tanaylab.github.io/dafr/reference/ReduceToColumn.md)

## Examples

``` r
ReduceToRow(Sum())
#> <DafrQuery> >- Sum 
```
