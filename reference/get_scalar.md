# Get scalar value from a Daf object

Retrieves the value of a scalar property with the given name from the
Daf data set.

## Usage

``` r
get_scalar(daf, name, default = NULL)
```

## Arguments

- daf:

  A Daf object

- name:

  Name of the scalar property to retrieve

- default:

  Default value to return if the scalar doesn't exist. If NULL, an error
  is thrown.

## Value

The scalar value or default if the property is not found

## Details

Numeric scalars are returned as integers or doubles, regardless of the
specific data type they are stored as in the Daf data set. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.get_scalar)
for details.
