# Set a vector on an axis.

Set a vector on an axis.

## Usage

``` r
set_vector(daf, axis, name, vec, overwrite = FALSE)
```

## Arguments

- daf:

  A `DafReader`.

- axis:

  Axis name.

- name:

  Vector name.

- vec:

  Atomic vector of length `axis_length(daf, axis)`, or a named vector
  whose names are a subset of the axis entries (reordered into axis
  order at storage time).

- overwrite:

  See `set_scalar`.

## Value

Invisibly the input `daf`.

## Examples

``` r
# Mirrors writers.jl jldoctest at line 300.
m <- example_metacells_daf()
has_vector(m, "type", "is_mebemp")                       # FALSE
#> [1] FALSE
set_vector(m, "type", "is_mebemp", c(TRUE, TRUE, FALSE, FALSE))
has_vector(m, "type", "is_mebemp")                       # TRUE
#> [1] TRUE
set_vector(m, "type", "is_mebemp",
           c(TRUE, TRUE, TRUE, FALSE), overwrite = TRUE)
has_vector(m, "type", "is_mebemp")                       # TRUE
#> [1] TRUE
```
