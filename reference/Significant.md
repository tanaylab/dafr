# Element-wise query operation: keep only "significant" entries.

Builds a `% Significant <params>` query fragment. Zeroes entries whose
absolute value is below `high`, optionally preserving entries between
`low` and `high` when any entry in the group is above `high`.

## Usage

``` r
Significant(type = NULL, ...)
```

## Arguments

- type:

  Optional output type (character scalar) or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Named parameters such as `high`, `low`; or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`Abs()`](https://tanaylab.github.io/dafr/reference/Abs.md),
[`Clamp()`](https://tanaylab.github.io/dafr/reference/Clamp.md)

## Examples

``` r
Significant(high = 3)
#> <DafrQuery> % Significant high: 3 
Significant(high = 3, low = 2)
#> <DafrQuery> % Significant high: 3 low: 2 
```
