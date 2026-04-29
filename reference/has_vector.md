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
# Mirrors readers.jl jldoctests at lines 525 + 533.
has_vector(example_cells_daf(),     "cell",     "type") # FALSE
#> [1] FALSE
has_vector(example_metacells_daf(), "metacell", "type") # TRUE
#> [1] TRUE
```
