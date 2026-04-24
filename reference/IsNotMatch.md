# Negated regex-match query operation.

Builds a `!~ <pattern>` query fragment. Filters entries whose property
value does NOT match the given regular-expression pattern.

## Usage

``` r
IsNotMatch(value, ...)
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

[`IsMatch()`](https://tanaylab.github.io/dafr/reference/IsMatch.md),
[`IsNotEqual()`](https://tanaylab.github.io/dafr/reference/IsNotEqual.md)

## Examples

``` r
IsNotMatch("^unknown")
#> <DafrQuery> !~ ^unknown 
Axis("cell") |> BeginMask("type") |> IsNotMatch("^unknown") |> EndMask()
#> <DafrQuery> @ cell [ type !~ ^unknown ] 
```
