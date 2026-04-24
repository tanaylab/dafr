# Test whether a vector exists on an axis.

Test whether a vector exists on an axis.

## Usage

``` r
has_vector(daf, axis, name)
```

## Arguments

- daf:

  A `DafReader`.

- axis:

  Axis name.

- name:

  Vector name.

## Value

Logical scalar.

## Examples

``` r
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2"))
set_vector(d, "cell", "donor", c("d1", "d2"))
has_vector(d, "cell", "donor")
#> [1] TRUE
has_vector(d, "cell", "age")
#> [1] FALSE
```
