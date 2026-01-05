# IfNot query operation

A query operation providing a value to use for "false-ish" values in a
vector. This replaces empty strings, zero numeric values, or false
Boolean values with the specified value. This is useful for handling
default or missing values in data without completely replacing the
property. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IfNot)
for details.

## Usage

``` r
IfNot(value = NULL, ...)
```

## Arguments

- value:

  Optional value to use for replacement. If NULL, uses the default
  replacement value.

- ...:

  Additional arguments needed to support usage of pipe operator

## Value

A query operation object that can be used in a query sequence
