# If-missing query operation.

Builds a `|| <default>` query fragment, providing a default value for
entries missing from the prior lookup. An optional `type` pins the
Julia-style dtype the default is coerced to when the lookup falls back
(`Int8`..`Int64`, `UInt8`..`UInt64`, `Float32`, `Float64`, `Bool`,
`String`).

## Usage

``` r
IfMissing(value, ..., type = NULL)
```

## Arguments

- value:

  Default value (character or numeric scalar), or a piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- ...:

  Optional piped
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

- type:

  Optional Julia-style dtype name (character scalar) to coerce the
  default to when it is actually used. `NULL` leaves the default as
  whatever the string parser emitted.

## Value

A [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md).

## See also

[`IfNot()`](https://tanaylab.github.io/dafr/reference/IfNot.md)

## Examples

``` r
IfMissing("N/A")
#> <DafrQuery> || "N/A" 
IfMissing(0)
#> <DafrQuery> || 0 
IfMissing(0, type = "Int64")
#> <DafrQuery> || 0 type Int64 
Axis("cell") |> LookupVector("age") |> IfMissing(0, type = "Int64")
#> <DafrQuery> @ cell : age || 0 type Int64 
```
