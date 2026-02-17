# Signal that an empty sparse vector has been filled

After obtaining an empty sparse vector via `get_empty_sparse_vector` and
filling in its non-zero indices and values, call this function to
finalize the vector and store it in the Daf object.

## Usage

``` r
filled_empty_sparse_vector(daf, axis, name, nzind, nzval)
```

## Arguments

- daf:

  A Daf object

- axis:

  Name of the axis

- name:

  Name of the vector property

- nzind:

  Vector of non-zero indices (1-based)

- nzval:

  Vector of non-zero values

## Value

The Daf object (invisibly, for chaining operations)

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.filled_empty_sparse_vector!)
for details.
