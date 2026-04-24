# Square-column-is query operation.

Builds a `@| <value>` query fragment, selecting the row of a square
matrix whose column key equals `value`.

## Usage

``` r
SquareColumnIs(value, ...)
```

## Arguments

- value:

  Column key (character or numeric scalar), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`SquareRowIs()`](https://tanaylab.github.io/dafr/reference/SquareRowIs.md)

## Examples

``` r
SquareColumnIs("M1")
#> <DafrQuery> @| M1 
```
