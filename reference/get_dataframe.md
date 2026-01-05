# Get a dataframe from a Daf object

Retrieves multiple vector properties for an axis as a dataframe.

## Usage

``` r
get_dataframe(daf, axis, columns = NULL, cache = FALSE)

get_tidy(daf, axis, columns = NULL, cache = FALSE, ...)
```

## Arguments

- daf:

  A Daf object

- axis:

  Axis name or query object

- columns:

  Vector of column specifications or named list/vector mapping column
  names to queries

- cache:

  Whether to cache the query results (FALSE by default)

- ...:

  Additional arguments passed to
  [`tidyr::pivot_longer`](https://tidyr.tidyverse.org/reference/pivot_longer.html)

## Value

A data.frame containing the specified columns for the axis, with row
names set to the axis entries. If columns is NULL, all columns are
returned with the "name" column removed if present.

For `get_tidy`, a tibble in long format with columns "name", "key", and
"value". The "name" column contains the axis entries, "key" contains the
column names, and "value" contains the corresponding values. Note that
if the types of the columns are not homogeneous, an error will be
thrown. Use the `values_transform` argument to transform the types of
the values, e.g. `values_transform = list(value = as.character)`.

## Details

This function allows retrieving multiple vectors for the same axis in a
single operation. The `columns` parameter can be a vector of vector
names, or a named list mapping output column names to vector names or
query strings. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.get_frame)
for more details.
