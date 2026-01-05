# Check if a scalar exists in a Daf object

Determines whether a scalar property with the specified name exists in
the Daf data set.

## Usage

``` r
has_scalar(daf, name)
```

## Arguments

- daf:

  A Daf object

- name:

  Name of the scalar property to check

## Value

TRUE if scalar exists, FALSE otherwise

## Details

Scalar properties are global values associated with the entire Daf data
set. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.has_scalar)
for details.
