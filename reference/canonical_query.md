# Canonicalise a query string.

Canonicalise a query string.

## Usage

``` r
canonical_query(query_string)
```

## Arguments

- query_string:

  A query string (character scalar) or a
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md)
  object produced by the query builders (e.g.
  `Axis("cell") |> LookupVector("donor")`).

## Value

The canonical query string (stable form; suitable for use as cache key).

## Examples

``` r
canonical_query("@ cell : donor")
#> [1] "@ cell : donor"
```
