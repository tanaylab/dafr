# Check if a query returns axis entries

Determines whether a query will return axis entries (names) as opposed
to scalar values, vectors, or matrices.

## Usage

``` r
is_axis_query(query)
```

## Arguments

- query:

  Query string or query object

## Value

TRUE if the query returns axis entries, FALSE otherwise

## Details

This is useful for determining the expected result type of a query
before executing it. A query that returns axis entries can be used as a
filter for other queries.
