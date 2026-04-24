# Return the dimensionality of a query's result.

Return the dimensionality of a query's result.

## Usage

``` r
query_result_dimensions(query_string)
```

## Arguments

- query_string:

  A query string (character scalar) or a
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md)
  object produced by the query builders (e.g.
  `Axis("cell") |> LookupVector("donor")`).

## Value

0L (scalar), 1L (vector / axis entries), 2L (matrix), or `NA_integer_`
if the query is ill-formed.

## Examples

``` r
query_result_dimensions(". organism")
#> [1] 0
query_result_dimensions("@ cell : donor")
#> [1] 1
query_result_dimensions("@ cell @ gene :: UMIs")
#> [1] 2
```
