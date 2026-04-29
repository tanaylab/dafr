# Set a scalar.

Set a scalar.

## Usage

``` r
set_scalar(daf, name, value, overwrite = FALSE)
```

## Arguments

- daf:

  A `DafReader`.

- name:

  Scalar name.

- value:

  Atomic scalar (length 1, non-NA).

- overwrite:

  If `FALSE` (default) error when the scalar already exists; if `TRUE`
  replace.

## Value

Invisibly the input `daf`.

## Examples

``` r
# Mirrors writers.jl jldoctest at line 63.
d <- example_cells_daf()
set_scalar(d, "version", 1.0)
get_scalar(d, "version")                          # 1.0
#> [1] 1
set_scalar(d, "version", 2.0, overwrite = TRUE)
get_scalar(d, "version")                          # 2.0
#> [1] 2
```
