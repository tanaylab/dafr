# Test whether an axis exists.

Test whether an axis exists.

## Usage

``` r
has_axis(daf, axis)
```

## Arguments

- daf:

  A `DafReader`.

- axis:

  Axis name (character scalar).

## Value

Logical scalar.

## Examples

``` r
# Mirrors readers.jl jldoctest at lines 210 + 218.
has_axis(example_cells_daf(),     "metacell") # FALSE
#> [1] FALSE
has_axis(example_metacells_daf(), "metacell") # TRUE
#> [1] TRUE
```
