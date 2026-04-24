# Check whether a query can be evaluated against a daf without error.

Check whether a query can be evaluated against a daf without error.

## Usage

``` r
has_query(daf, query_string)
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

Logical scalar. TRUE if `get_query(daf, query_string)` would succeed
with a non-empty result; FALSE otherwise.

## Examples

``` r
d <- example_cells_daf()
has_query(d, "@ cell : donor")
#> [1] TRUE
has_query(d, "@ cell : nonexistent_property")
#> [1] FALSE
```
