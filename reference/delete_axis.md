# Delete an axis (and all vectors / matrices that depend on it).

Delete an axis (and all vectors / matrices that depend on it).

## Usage

``` r
delete_axis(daf, axis, must_exist = TRUE)
```

## Arguments

- daf:

  A `DafWriter`.

- axis:

  Axis name.

- must_exist:

  If `TRUE` (default) raise when the axis is absent; if `FALSE` silently
  no-op.

## Value

Invisibly the input `daf`.

## Examples

``` r
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2"))
add_axis(d, "gene", c("g1", "g2", "g3"))
delete_axis(d, "gene")
axes_set(d)
#> [1] "cell"
```
