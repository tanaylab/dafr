# Get the number of dimensions of a query result

Determines the dimensionality of the result that would be produced by
applying a query. This is useful for understanding what kind of data
structure to expect from a query before executing it, which can help
with writing code that handles different result types.

## Usage

``` r
query_result_dimensions(query)
```

## Arguments

- query:

  Query string or object

## Value

Number of dimensions (-1 - names, 0 - scalar, 1 - vector, 2 - matrix)

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html)
for details.
