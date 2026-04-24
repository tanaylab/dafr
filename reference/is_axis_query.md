# Test whether a query yields an axis entry vector.

Test whether a query yields an axis entry vector.

## Usage

``` r
is_axis_query(query_string)
```

## Arguments

- query_string:

  A query string (character scalar) or a
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md)
  object produced by the query builders (e.g.
  `Axis("cell") |> LookupVector("donor")`).

## Value

Logical scalar.

## Examples

``` r
is_axis_query("@ cell")
#> [1] TRUE
is_axis_query(". organism")
#> [1] FALSE
```
