# Equal-to comparison query operation.

Builds a `= <value>` query fragment. Typically used inside a mask
subquery (after
[`BeginMask()`](https://tanaylab.github.io/dafr/reference/BeginMask.md))
to filter entries whose property equals `value`.

## Usage

``` r
IsEqual(value, ...)
```

## Arguments

- value:

  Value to compare against (character or numeric scalar), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`IsNotEqual()`](https://tanaylab.github.io/dafr/reference/IsNotEqual.md),
[`IsGreater()`](https://tanaylab.github.io/dafr/reference/IsGreater.md),
[`IsLess()`](https://tanaylab.github.io/dafr/reference/IsLess.md),
[`IsMatch()`](https://tanaylab.github.io/dafr/reference/IsMatch.md)

## Examples

``` r
IsEqual("T-cell")
#> <DafrQuery> = T-cell 
Axis("cell") |> BeginMask("type") |> IsEqual("T-cell") |> EndMask()
#> <DafrQuery> @ cell [ type = T-cell ] 
```
