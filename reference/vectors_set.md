# Names of vectors on an axis, sorted.

Names of vectors on an axis, sorted.

## Usage

``` r
vectors_set(daf, axis)
```

## Arguments

- daf:

  A `DafReader`.

- axis:

  Axis name.

## Value

Character vector.

## Examples

``` r
# Mirrors readers.jl jldoctest at line 595.
vectors_set(example_cells_daf(), "cell") # "donor" "experiment"
#> [1] "donor"      "experiment"
```
