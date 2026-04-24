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
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2"))
set_vector(d, "cell", "donor", c("d1", "d2"))
delete_vector(d, "cell", "donor")
has_vector(d, "cell", "donor")
#> [1] FALSE
```
