# Set vector in a Daf object

Sets a vector property with the specified name for an axis in the Daf
data set.

## Usage

``` r
set_vector(daf, axis, name, value, overwrite = FALSE)
```

## Arguments

- daf:

  A Daf object

- axis:

  Axis name

- name:

  Name of the vector property

- value:

  Vector of values to set (cannot contain NA values)

- overwrite:

  Whether to overwrite if vector already exists (FALSE by default)

## Value

The Daf object (invisibly, for chaining operations)

## Details

This function creates or updates a vector property in the Daf data set.
The length of the vector must match the length of the axis. If the
vector already exists and `overwrite` is FALSE, an error will be raised.
NA values are not supported in Daf. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.set_vector!)
for details.
