# Evaluate a query against a daf reader.

Evaluate a query against a daf reader.

## Usage

``` r
get_query(daf, query_string)
```

## Arguments

- daf:

  A `DafReader`.

- query_string:

  A query string (character scalar) or a
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md)
  object produced by the query builders (e.g.
  `Axis("cell") |> LookupVector("donor")`).

## Value

A scalar, vector, matrix, names set, or NULL if missing.

## Examples

``` r
d <- example_cells_daf()
get_query(d, ". organism")
#> [1] "human"
head(get_query(d, "@ cell : donor"))
#> [1] "N89" "N84" "N86" "N84" "N89" "N89"
```
