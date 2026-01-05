# Extract results from a Daf object using a query

Extract results from a Daf object using a query

## Usage

``` r
# S3 method for class 'Daf'
x[i, ...]
```

## Arguments

- x:

  A Daf object

- i:

  A query string or object

- ...:

  Ignored. Present for compatibility with the `[` generic.

## Value

The result of the query

## Details

The expression `daf[query]` is equivalent to
`get_query(daf, query, cache = FALSE)`. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.get_query)
for details.
