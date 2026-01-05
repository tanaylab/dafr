# Delete scalar from a Daf object

Removes a scalar property with the specified name from the Daf data set.

## Usage

``` r
delete_scalar(daf, name, must_exist = TRUE)
```

## Arguments

- daf:

  A Daf object

- name:

  Name of the scalar property to delete

- must_exist:

  Whether to error if scalar doesn't exist (TRUE by default)

## Value

The Daf object (invisibly, for chaining operations)

## Details

If `must_exist` is TRUE and the scalar doesn't exist, an error will be
raised. Otherwise, the function will silently succeed even if the scalar
doesn't exist. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.delete_scalar!)
for details.
