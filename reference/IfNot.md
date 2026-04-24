# If-not query operation.

Builds a `??` (optionally `?? <value>`) query fragment, providing a
fallback when a prior boolean query yields `FALSE`.

## Usage

``` r
IfNot(value = NULL, ...)
```

## Arguments

- value:

  Optional fallback value (character scalar), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`IfMissing()`](https://tanaylab.github.io/dafr/reference/IfMissing.md)

## Examples

``` r
IfNot()
#> <DafrQuery> ?? 
IfNot("fallback")
#> <DafrQuery> ?? fallback 
```
