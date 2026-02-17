# Get an empty sparse matrix for filling

Returns an empty sparse matrix for the specified axes and property,
which can be filled in-place. This is useful for efficiently
constructing large sparse matrices. After filling with
`filled_empty_sparse_matrix`, the matrix is stored in the Daf object.

## Usage

``` r
get_empty_sparse_matrix(
  daf,
  rows_axis,
  columns_axis,
  name,
  eltype,
  nnz,
  indtype = "Int64",
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

  Element type for the matrix values (e.g., "Float64", "Int32")

- nnz:

  Number of non-zero elements expected

- indtype:

  Optional index type (e.g., "Int32"). If NULL, the default is used.

- overwrite:

  Whether to overwrite if matrix already exists (FALSE by default)

## Value

A Julia sparse matrix object that can be filled in-place

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.get_empty_sparse_matrix!)
for details.
