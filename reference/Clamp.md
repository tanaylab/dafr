# Element-wise query operation: clamp values to a range.

Builds a `% Clamp <params>` query fragment. Values less than `min`
become `min`; values greater than `max` become `max`.

## Usage

``` r
Clamp(type = NULL, ...)
```

## Arguments

- type:

  Optional output type (character scalar) or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Named parameters `min`, `max`; or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`Significant()`](https://tanaylab.github.io/dafr/reference/Significant.md),
[`Convert()`](https://tanaylab.github.io/dafr/reference/Convert.md)

## Examples

``` r
Clamp(min = 0, max = 1)
#> <DafrQuery> % Clamp min: 0 max: 1 
```
