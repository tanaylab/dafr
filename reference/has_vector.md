# Check if a vector exists in a Daf object

Determines whether a vector property with the specified name exists for
the given axis.

## Usage

``` r
has_vector(daf, axis, name)
```

## Arguments

- daf:

  A Daf object

- axis:

  Name of the axis

- name:

  Name of the vector property

## Value

TRUE if vector exists, FALSE otherwise

## Details

Vector properties store one-dimensional data along a specific axis. Each
entry in the axis has a corresponding value in the vector. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.has_vector)
for details.
