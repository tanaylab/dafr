# Check if a query requires relayout

Determines whether executing a query on a Daf object would require a
matrix relayout operation. This is useful for performance optimization,
as relayout can be expensive.

## Usage

``` r
query_requires_relayout(daf, query)
```

## Arguments

- daf:

  A Daf object

- query:

  Query string or query object

## Value

TRUE if the query requires relayout, FALSE otherwise

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.query_requires_relayout)
for details.
