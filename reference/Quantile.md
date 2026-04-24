# Reduction query operation: quantile.

Builds a `% Quantile p: <p>` query fragment. The `p` parameter must be
provided by name (positional first argument is `type`).

## Usage

``` r
Quantile(type = NULL, ...)
```

## Arguments

- type:

  Optional output type (character scalar) or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Named parameter `p` (quantile in 0..1); or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`Median()`](https://tanaylab.github.io/dafr/reference/Median.md)

## Examples

``` r
Quantile(p = 0.5)
#> <DafrQuery> % Quantile p: "0.5" 
Quantile(p = 0.9, type = "Float64")
#> <DafrQuery> % Quantile type: Float64 p: "0.9" 
```
