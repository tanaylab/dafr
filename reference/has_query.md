# Check if a query can be applied to a Daf object

Determines whether a query can be validly applied to a Daf object. This
is useful for checking if the properties referenced in a query exist in
the Daf object before attempting to execute the query.

## Usage

``` r
has_query(daf, query)
```

## Arguments

- daf:

  A Daf object

- query:

  Query string or object. Can be created using query operations such as
  Axis(), Lookup(), IsGreater(), etc.

## Value

TRUE if query can be applied, FALSE otherwise

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.has_query)
for details.

## See also

Axis, Lookup, Names, AsAxis, Fetch, IfMissing, IfNot, MaskSlice,
SquareMaskColumn, SquareMaskRow, And, AndNot, Or, OrNot, Xor, XorNot,
IsEqual, IsNotEqual, IsLess, IsLessEqual, IsGreater, IsGreaterEqual,
IsMatch, IsNotMatch, CountBy, GroupBy and other query operations.
