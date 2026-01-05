# Apply a query to a Daf object and return result as a data.frame

Executes a query on a Daf object and returns the result formatted as a
data.frame for easier use in R analysis workflows. This is a convenience
wrapper around get_query that provides properly structured dataframes
for different result types.

## Usage

``` r
get_dataframe_query(daf = NULL, query = NULL, cache = TRUE)
```

## Arguments

- daf:

  A Daf object

- query:

  Query string or object. Can be created using query operations such as
  Axis(), Lookup(), IsGreater(), etc. In order to support the use of
  pipe operators, the query can also be a Daf object and vice versa.

- cache:

  Whether to cache the query result

## Value

A data.frame representation of the query result. If the query result is
a matrix, row names and column names are the axis entries. If the query
result is a scalar, a data.frame with one row and one column is
returned. If the query result is a vector, a data.frame with a column
"value" is returned.

## See also

get_query, get_frame
