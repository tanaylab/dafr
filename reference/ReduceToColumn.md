# Reduce-to-column query operation.

Builds a `>| <Reduction>` query fragment from a reduction query.
Converts each row of a matrix to a single value via the given reduction
([`Sum()`](https://tanaylab.github.io/dafr/reference/Sum.md),
[`Mean()`](https://tanaylab.github.io/dafr/reference/Mean.md),
`Quantile(p = 0.5)`, ...).

## Usage

``` r
ReduceToColumn(reduction, ...)
```

## Arguments

- reduction:

  A reduction
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md)
  (e.g. [`Sum()`](https://tanaylab.github.io/dafr/reference/Sum.md),
  `Quantile(p = 0.5)`), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md)
  with the reduction supplied in `...`.

- ...:

  The reduction
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md)
  when `reduction` holds the piped prior.

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`ReduceToRow()`](https://tanaylab.github.io/dafr/reference/ReduceToRow.md)

## Examples

``` r
ReduceToColumn(Sum())
#> <DafrQuery> >| Sum 
ReduceToColumn(Quantile(p = 0.5))
#> <DafrQuery> >| Quantile p: "0.5" 
```
