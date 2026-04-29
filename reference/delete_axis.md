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
# Mirrors writers.jl jldoctest at line 217.
m <- example_metacells_daf()
has_axis(m, "type")    # TRUE
#> [1] TRUE
delete_axis(m, "type")
has_axis(m, "type")    # FALSE
#> [1] FALSE
```
