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
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2", "c3"))
set_vector(d, "cell", "donor", c("d1", "d2", "d1"))
get_vector(d, "cell", "donor")
#>   c1   c2   c3 
#> "d1" "d2" "d1" 
```
