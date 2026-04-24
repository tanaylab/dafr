# Lookup-scalar query operation.

Builds a `.` (optionally `. <name>`) query fragment, looking up the
named scalar property.

## Usage

``` r
LookupScalar(value = NULL, ...)
```

## Arguments

- value:

  Optional scalar name (character scalar), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`LookupVector()`](https://tanaylab.github.io/dafr/reference/LookupVector.md),
[`LookupMatrix()`](https://tanaylab.github.io/dafr/reference/LookupMatrix.md)

## Examples

``` r
LookupScalar()
#> <DafrQuery> . 
LookupScalar("version")
#> <DafrQuery> . version 
```
