# Get an empty dense matrix for filling

Returns an empty dense matrix for the specified axes and property, which
can be filled in-place. This is useful for efficiently constructing
large matrices without allocating temporary storage. After filling, the
matrix is automatically stored in the Daf object.

## Usage

``` r
get_empty_dense_matrix(
  daf,
  rows_axis,
  columns_axis,
  name,
  eltype,
  overwrite = FALSE
)
```

## Arguments

- daf:

  A Daf object

- rows_axis:

  Name of the rows axis

- columns_axis:

  Name of the columns axis

- name:

  Name of the matrix property

- eltype:

  Element type for the matrix (e.g., "Float64", "Int32")

- overwrite:

  Whether to overwrite if matrix already exists (FALSE by default)

## Value

A Julia matrix object that can be filled in-place

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.get_empty_dense_matrix!)
for details.
