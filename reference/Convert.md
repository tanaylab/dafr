# Element-wise query operation: convert values to a given type.

Builds a `% Convert type <value>` query fragment.

## Usage

``` r
Convert(type = NULL, ...)
```

## Arguments

- type:

  Target type as a character scalar (e.g. `"Int64"`, `"Float32"`).
  Positional.

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`Clamp()`](https://tanaylab.github.io/dafr/reference/Clamp.md),
[`Round()`](https://tanaylab.github.io/dafr/reference/Round.md)

## Examples

``` r
Convert(type = "Int64")
#> <DafrQuery> % Convert type: Int64 
```
