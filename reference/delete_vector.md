# Delete a vector on an axis.

Delete a vector on an axis.

## Usage

``` r
delete_vector(daf, axis, name, must_exist = TRUE)
```

## Arguments

- daf:

  A `DafReader`.

- axis:

  Axis name.

- name:

  Vector name.

- must_exist:

  See `delete_axis`.

## Value

Invisibly the input `daf`.

## Examples

``` r
# Mirrors writers.jl jldoctest at line 618.
m <- example_metacells_daf()
has_vector(m, "type", "color")    # TRUE
#> [1] TRUE
delete_vector(m, "type", "color")
has_vector(m, "type", "color")    # FALSE
#> [1] FALSE
```
