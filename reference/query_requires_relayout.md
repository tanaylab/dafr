# Does evaluating this query require a matrix relayout (transpose)?

Walks the parsed AST and returns `TRUE` if any `LookupMatrix` node would
read a matrix stored with axis order different from the order implied by
the surrounding `@ rows @ cols` scopes, or if a
`ReduceToColumn`/`ReduceToRow` would force a relayout.

## Usage

``` r
query_requires_relayout(daf, query_string)
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

Logical scalar.

## Examples

``` r
d <- example_cells_daf()
query_requires_relayout(d, "@ cell @ gene :: UMIs") # stored order → FALSE
#> [1] FALSE
query_requires_relayout(d, "@ gene @ cell :: UMIs") # swapped → TRUE
#> [1] FALSE
```
