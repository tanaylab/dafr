# Add a new axis.

Add a new axis.

## Usage

``` r
add_axis(daf, axis, entries)
```

## Arguments

- daf:

  A `DafWriter`.

- axis:

  Axis name.

- entries:

  Unique, non-NA, non-empty character vector of entry names.

## Value

Invisibly the input `daf`.

## Examples

``` r
# Mirrors writers.jl jldoctest at line 161.
d <- example_cells_daf()
has_axis(d, "block")                  # FALSE
#> [1] FALSE
add_axis(d, "block", c("B1", "B2"))
has_axis(d, "block")                  # TRUE
#> [1] TRUE
```
