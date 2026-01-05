# Delete vector from a Daf object

Removes a vector property with the specified name from the Daf data set.

## Usage

``` r
delete_vector(daf, axis, name, must_exist = TRUE)
```

## Arguments

- daf:

  A Daf object

- axis:

  Axis name

- name:

  Name of the vector property to delete

- must_exist:

  Whether to error if vector doesn't exist (TRUE by default)

## Value

The Daf object (invisibly, for chaining operations)

## Details

If `must_exist` is TRUE and the vector doesn't exist, an error will be
raised. Otherwise, the function will silently succeed even if the vector
doesn't exist. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.delete_vector!)
for details.
