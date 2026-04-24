# Return the axis name implied by a query, if any.

Return the axis name implied by a query, if any.

## Usage

``` r
query_axis_name(query_string)
```

## Arguments

- query_string:

  A query string (character scalar) or a
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md)
  object produced by the query builders (e.g.
  `Axis("cell") |> LookupVector("donor")`).

## Value

A single axis name, or `NA_character_` if the query references 0 or 2+
axes.

## Examples

``` r
query_axis_name("@ cell : donor")
#> [1] "cell"
query_axis_name(". organism")
#> [1] NA
```
