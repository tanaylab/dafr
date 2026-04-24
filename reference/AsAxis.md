# As-axis query operation.

Builds a `=@` (optionally `=@ <axis_name>`) query fragment, treating the
prior vector's values as entries of an axis.

## Usage

``` r
AsAxis(value = NULL, ...)
```

## Arguments

- value:

  Optional axis name (character scalar), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`Axis()`](https://tanaylab.github.io/dafr/reference/Axis.md)

## Examples

``` r
AsAxis()
#> <DafrQuery> =@ 
AsAxis("cell")
#> <DafrQuery> =@ cell 
Axis("cell") |> LookupVector("donor") |> AsAxis()
#> <DafrQuery> @ cell : donor =@ 
```
