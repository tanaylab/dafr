# If-missing query operation.

Builds a `|| <default>` query fragment, providing a default value for
entries missing from the prior lookup.

## Usage

``` r
IfMissing(value, ...)
```

## Arguments

- value:

  Default value (character or numeric scalar), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`IfNot()`](https://tanaylab.github.io/dafr/reference/IfNot.md)

## Examples

``` r
IfMissing("N/A")
#> <DafrQuery> || "N/A" 
IfMissing(0)
#> <DafrQuery> || 0 
Axis("cell") |> LookupVector("age") |> IfMissing(0)
#> <DafrQuery> @ cell : age || 0 
```
