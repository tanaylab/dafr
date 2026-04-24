# Pivot axis vectors into a tidy long-format tibble.

Requires `tidyr` and `tibble`; errors with an install hint if either is
missing.

## Usage

``` r
get_tidy(daf, axis, columns = NULL, cache = TRUE, ...)
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

- ...:

  Passed to
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html).

## Value

A `tibble` with columns `name`, `key`, `value`.

## See also

[`get_dataframe()`](https://tanaylab.github.io/dafr/reference/get_dataframe.md),
[`get_dataframe_query()`](https://tanaylab.github.io/dafr/reference/get_dataframe_query.md)

## Examples

``` r
if (requireNamespace("tidyr", quietly = TRUE) &&
    requireNamespace("tibble", quietly = TRUE)) {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("A", "B"))
    get_tidy(d, "cell")
}
#> # A tibble: 2 × 3
#>   name  key   value
#>   <chr> <chr> <chr>
#> 1 c1    donor A    
#> 2 c2    donor B    
```
