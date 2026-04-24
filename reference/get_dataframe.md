# Extract vectors on an axis as a `data.frame`.

Returns a `data.frame` with one column per vector on `axis`, rows named
by the axis entries. When `columns` is `NULL`, all vectors defined on
`axis` are included.

## Usage

``` r
get_dataframe(daf, axis, columns = NULL, cache = TRUE)
```

## Arguments

- daf:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md).

- axis:

  Axis name.

- columns:

  Optional character vector of vector names to include. Defaults to all
  vectors on `axis`.

- cache:

  Logical; if `TRUE` (default), the result is memoised in the query
  cache and served on repeat calls.

## Value

A `data.frame`.

## See also

[`get_dataframe_query()`](https://tanaylab.github.io/dafr/reference/get_dataframe_query.md),
[`get_tidy()`](https://tanaylab.github.io/dafr/reference/get_tidy.md)

## Examples

``` r
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2"))
set_vector(d, "cell", "donor", c("A", "B"))
get_dataframe(d, "cell")
#>    donor
#> c1     A
#> c2     B
```
