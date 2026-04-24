# Not-equal-to comparison query operation.

Builds a `!= <value>` query fragment.

## Usage

``` r
IsNotEqual(value, ...)
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

[`IsEqual()`](https://tanaylab.github.io/dafr/reference/IsEqual.md),
[`IsMatch()`](https://tanaylab.github.io/dafr/reference/IsMatch.md),
[`IsNotMatch()`](https://tanaylab.github.io/dafr/reference/IsNotMatch.md)

## Examples

``` r
IsNotEqual("unknown")
#> <DafrQuery> != unknown 
```
