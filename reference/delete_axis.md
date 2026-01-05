# Delete axis from a Daf object

Removes an axis and all its associated data from the Daf data set.

## Usage

``` r
delete_axis(daf, axis, must_exist = TRUE)
```

## Arguments

- daf:

  A Daf object

- axis:

  Name of the axis to delete

- must_exist:

  Whether to error if axis doesn't exist (TRUE by default)

## Value

The Daf object (invisibly, for chaining operations)

## Details

This function deletes an axis and all vector and matrix properties
associated with it. If `must_exist` is TRUE and the axis doesn't exist,
an error will be raised. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.delete_axis!)
for details.
