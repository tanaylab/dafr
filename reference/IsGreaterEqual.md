# Greater-than-or-equal comparison query operation.

Builds a `>= <value>` query fragment.

## Usage

``` r
IsGreaterEqual(value, ...)
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

[`IsGreater()`](https://tanaylab.github.io/dafr/reference/IsGreater.md),
[`IsLessEqual()`](https://tanaylab.github.io/dafr/reference/IsLessEqual.md)

## Examples

``` r
IsGreaterEqual(18)
#> <DafrQuery> >= 18 
```
