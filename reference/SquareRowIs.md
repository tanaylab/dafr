# Square-row-is query operation.

Builds a `@- <value>` query fragment, selecting the column of a square
matrix whose row key equals `value`.

## Usage

``` r
SquareRowIs(value, ...)
```

## Arguments

- value:

  Row key (character or numeric scalar), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`SquareColumnIs()`](https://tanaylab.github.io/dafr/reference/SquareColumnIs.md)

## Examples

``` r
SquareRowIs("M1")
#> <DafrQuery> @- M1 
```
