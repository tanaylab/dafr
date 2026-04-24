# Less-than comparison query operation.

Builds a `< <value>` query fragment.

## Usage

``` r
IsLess(value, ...)
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

[`IsLessEqual()`](https://tanaylab.github.io/dafr/reference/IsLessEqual.md),
[`IsGreater()`](https://tanaylab.github.io/dafr/reference/IsGreater.md)

## Examples

``` r
IsLess(100)
#> <DafrQuery> < 100 
Axis("cell") |> BeginMask("score") |> IsLess(100) |> EndMask()
#> <DafrQuery> @ cell [ score < 100 ] 
```
