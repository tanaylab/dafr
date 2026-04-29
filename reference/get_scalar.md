# Get a scalar, optionally with a default when missing.

Get a scalar, optionally with a default when missing.

## Usage

``` r
get_scalar(daf, name, default)
```

## Arguments

- daf:

  A `DafReader`.

- name:

  Scalar name.

- default:

  Value to return when the scalar is absent. If missing and the scalar
  is absent, an error is raised.

## Value

The scalar value.

## Examples

``` r
# Mirrors readers.jl jldoctests at lines 157 + 165.
get_scalar(example_cells_daf(), "organism") # "human"
#> [1] "human"
get_scalar(example_metacells_daf(), "organism", default = NULL) # NULL
#> NULL
```
