# Signal that an empty sparse matrix has been filled

After obtaining an empty sparse matrix via `get_empty_sparse_matrix` and
filling in its column pointers, row values, and non-zero values, call
this function to finalize the matrix and store it in the Daf object.

## Usage

``` r
filled_empty_sparse_matrix(
  daf,
  rows_axis,
  columns_axis,
  name,
  colptr,
  rowval,
  nzval
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

- colptr:

  Vector of column pointers (1-based)

- rowval:

  Vector of row indices for non-zero values (1-based)

- nzval:

  Vector of non-zero values

## Value

The Daf object (invisibly, for chaining operations)

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.filled_empty_sparse_matrix!)
for details.
