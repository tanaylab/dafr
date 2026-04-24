# Reduction query operation: sum.

Builds a `% Sum` query fragment (optionally `% Sum type: <T>`).

## Usage

``` r
Sum(type = NULL, ...)
```

## Arguments

- type:

  Optional output type (character scalar) or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`Mean()`](https://tanaylab.github.io/dafr/reference/Mean.md),
[`Max()`](https://tanaylab.github.io/dafr/reference/Max.md)

## Examples

``` r
Sum()
#> <DafrQuery> % Sum 
Sum(type = "Float64")
#> <DafrQuery> % Sum type: Float64 
```
