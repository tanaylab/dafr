# Regex-match query operation.

Builds a `~ <pattern>` query fragment. Filters entries whose property
value matches the given regular-expression pattern.

## Usage

``` r
IsMatch(value, ...)
```

## Arguments

- value:

  Regex pattern (character scalar), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`IsNotMatch()`](https://tanaylab.github.io/dafr/reference/IsNotMatch.md),
[`IsEqual()`](https://tanaylab.github.io/dafr/reference/IsEqual.md)

## Examples

``` r
IsMatch("^T")
#> <DafrQuery> ~ ^T 
Axis("cell") |> BeginMask("type") |> IsMatch("^T") |> EndMask()
#> <DafrQuery> @ cell [ type ~ ^T ] 
```
