# Apply a query to a Daf object

Executes a query on a Daf object and returns the result. Queries provide
a way to extract, filter, and manipulate data from a Daf object using a
composable syntax. Queries can retrieve scalars, vectors, matrices, or
sets of names depending on the operations used.

## Usage

``` r
get_query(daf = NULL, query = NULL, cache = TRUE)
```

## Arguments

- daf:

  A Daf object

- query:

  Query string or object. Can be created using query operations such as
  Axis(), Lookup(), IsGreater(), etc. In order to support the use of
  pipe operators, the query can also be a Daf object and vice versa, see
  examples below.

- cache:

  Whether to cache the query result

## Value

The result of the query, which could be a scalar, vector, matrix, or set
of names depending on the query

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.get_query)
for details.

## See also

Axis, Lookup, Names, AsAxis, Fetch, IfMissing, IfNot, MaskSlice,
SquareMaskColumn, SquareMaskRow, And, AndNot, Or, OrNot, Xor, XorNot,
IsEqual, IsNotEqual, IsLess, IsLessEqual, IsGreater, IsGreaterEqual,
IsMatch, IsNotMatch, CountBy, GroupBy and other query operations.
