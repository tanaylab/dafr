# Element-wise query operation: absolute value.

Builds a `% Abs` query fragment. Chain after a vector or matrix lookup
via `|>` to apply element-wise absolute value.

## Usage

``` r
Abs(...)
```

## Arguments

- ...:

  Optional
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md)
  pipe target.

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`Round()`](https://tanaylab.github.io/dafr/reference/Round.md),
[`Log()`](https://tanaylab.github.io/dafr/reference/Log.md),
[`Clamp()`](https://tanaylab.github.io/dafr/reference/Clamp.md)

## Examples

``` r
Abs()
#> <DafrQuery> % Abs 
DafrQuery(ast = parse_query("@ cell : age"), canonical = "@ cell : age") |>
    Abs()
#> <DafrQuery> @ cell : age % Abs 
```
