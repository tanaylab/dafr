# Greater-than comparison query operation.

Builds a `> <value>` query fragment.

## Usage

``` r
IsGreater(value, ...)
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

[`IsGreaterEqual()`](https://tanaylab.github.io/dafr/reference/IsGreaterEqual.md),
[`IsLess()`](https://tanaylab.github.io/dafr/reference/IsLess.md),
[`IsLessEqual()`](https://tanaylab.github.io/dafr/reference/IsLessEqual.md)

## Examples

``` r
IsGreater(18)
#> <DafrQuery> > 18 
Axis("cell") |> BeginMask("age") |> IsGreater(18) |> EndMask()
#> <DafrQuery> @ cell [ age > 18 ] 
```
