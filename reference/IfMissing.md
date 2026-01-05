# IfMissing query operation

A query operation providing a default value to use if the data is
missing some property. This is useful when querying for properties that
might not exist for all entries, allowing you to provide a fallback
value instead of getting an error. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IfMissing)
for details.

## Usage

``` r
IfMissing(missing_value, type = NULL, ...)
```

## Arguments

- missing_value:

  Value to use when data is missing the property

- type:

  Optional type specification for the missing value (e.g., "Int64",
  "Float64", "Bool")

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
