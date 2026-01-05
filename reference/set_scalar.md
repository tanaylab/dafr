# Set scalar value in a Daf object

Sets the value of a scalar property with the specified name in the Daf
data set.

## Usage

``` r
set_scalar(daf, name, value, overwrite = FALSE)
```

## Arguments

- daf:

  A Daf object

- name:

  Name of the scalar property

- value:

  Value to set (cannot be NA)

- overwrite:

  Whether to overwrite if scalar already exists (FALSE by default)

## Value

The Daf object (invisibly, for chaining operations)

## Details

This function creates or updates a scalar property in the Daf data set.
If the scalar already exists and `overwrite` is FALSE, an error will be
raised. NA values are not supported in Daf. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.set_scalar!)
for details.
