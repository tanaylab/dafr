# Get vector from a Daf object

Retrieves a vector property with the specified name for a given axis.

## Usage

``` r
get_vector(daf, axis, name, default = NULL)
```

## Arguments

- daf:

  A Daf object

- axis:

  Axis name

- name:

  Name of the vector property

- default:

  Default value if vector doesn't exist (NULL by default)

## Value

A named vector containing the property values, with names set to the
axis entry names, or the default value if the property doesn't exist

## Details

Vector properties store one-dimensional data along an axis, with one
value for each entry in the axis. If the vector doesn't exist and
default is NA, a vector of NAs with appropriate length is returned. See
the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.get_vector)
for details.
