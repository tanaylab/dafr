# Less-than-or-equal comparison query operation.

Builds a `<= <value>` query fragment.

## Usage

``` r
IsLessEqual(value, ...)
```

## Arguments

- value:

  Threshold value (character or numeric scalar), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`IsLess()`](https://tanaylab.github.io/dafr/reference/IsLess.md),
[`IsGreaterEqual()`](https://tanaylab.github.io/dafr/reference/IsGreaterEqual.md)

## Examples

``` r
IsLessEqual(65)
#> <DafrQuery> <= 65 
```
