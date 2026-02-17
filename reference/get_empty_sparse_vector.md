# Get an empty sparse vector for filling

Returns an empty sparse vector for the specified axis and property,
which can be filled in-place. This is useful for efficiently
constructing large sparse vectors. After filling with
`filled_empty_sparse_vector`, the vector is stored in the Daf object.

## Usage

``` r
get_empty_sparse_vector(
  daf,
  axis,
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

- axis:

  Name of the axis

- name:

  Name of the vector property

- eltype:

  Element type for the vector values (e.g., "Float64", "Int32")

- nnz:

  Number of non-zero elements expected

- indtype:

  Optional index type (e.g., "Int32"). If NULL, the default is used.

- overwrite:

  Whether to overwrite if vector already exists (FALSE by default)

## Value

A Julia sparse vector object that can be filled in-place

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.get_empty_sparse_vector!)
for details.
