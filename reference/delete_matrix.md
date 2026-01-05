# Delete matrix from a Daf object

Removes a matrix property with the specified name from the Daf data set.

## Usage

``` r
delete_matrix(daf, rows_axis, columns_axis, name, must_exist = TRUE)
```

## Arguments

- daf:

  A Daf object

- rows_axis:

  Name of rows axis

- columns_axis:

  Name of columns axis

- name:

  Name of the matrix property to delete

- must_exist:

  Whether to error if matrix doesn't exist (TRUE by default)

## Value

The Daf object (invisibly, for chaining operations)

## Details

If `must_exist` is TRUE and the matrix doesn't exist, an error will be
raised. Otherwise, the function will silently succeed even if the matrix
doesn't exist. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.delete_matrix!)
for details.
