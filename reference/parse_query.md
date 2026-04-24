# Parse a query string into an AST (list of `qop` nodes).

Parse a query string into an AST (list of `qop` nodes).

## Usage

``` r
parse_query(query_string)
```

## Arguments

- query_string:

  A character scalar.

## Value

A list of AST node records.

## Examples

``` r
# Most users call get_query() directly; parse_query() returns the AST.
ast <- parse_query("@ cell : donor")
is_axis_query("@ cell : donor")
#> [1] FALSE
get_query(example_cells_daf(), "@ cell : donor") |> head()
#> [1] "N89" "N84" "N86" "N84" "N89" "N89"
```
