# Test whether a scalar exists.

Test whether a scalar exists.

## Usage

``` r
has_scalar(daf, name)
```

## Arguments

- daf:

  A `DafReader`.

- name:

  Scalar name.

## Value

Logical scalar.

## Examples

``` r
# Mirrors readers.jl jldoctests at lines 92 + 100.
has_scalar(example_cells_daf(),     "organism") # TRUE
#> [1] TRUE
has_scalar(example_metacells_daf(), "organism") # FALSE
#> [1] FALSE
```
