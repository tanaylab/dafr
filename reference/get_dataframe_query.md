# Extract an axis-resolving query's result as a `data.frame`.

The query-string counterpart of
[`get_dataframe()`](https://tanaylab.github.io/dafr/reference/get_dataframe.md).
The query must resolve to an axis (possibly mask-filtered).

## Usage

``` r
get_dataframe_query(daf, query, columns = NULL, cache = TRUE)
```

## Arguments

- daf:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md).

- query:

  A query string resolving to an axis.

- columns:

  Optional character vector of column names to include in the result.
  Defaults to all vectors on the resolved axis.

- cache:

  Logical; if `TRUE` (default), serve from the query cache.

## Value

A `data.frame` with axis-entry rownames.

## See also

[`get_dataframe()`](https://tanaylab.github.io/dafr/reference/get_dataframe.md),
[`get_tidy()`](https://tanaylab.github.io/dafr/reference/get_tidy.md)

## Examples

``` r
d <- memory_daf()
add_axis(d, "donor", c("d1", "d2"))
set_vector(d, "donor", "age", c(20L, 30L))
get_dataframe_query(d, "@ donor")
#>    age
#> d1  20
#> d2  30
```
