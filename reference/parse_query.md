# Parse a query string into a query object

Converts a string representation of a query into a query object that can
be applied to a Daf object. This allows for a more concise syntax for
creating complex queries. If you want something similar to the `q`
prefix used in Julia, you can write `q <- parse_query` in your R code.

## Usage

``` r
parse_query(query_string)
```

## Arguments

- query_string:

  String containing the query

## Value

A query object that can be used with get_query and has_query

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html)
for details.
