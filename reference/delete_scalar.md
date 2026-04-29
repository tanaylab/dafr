# Delete a scalar.

Delete a scalar.

## Usage

``` r
delete_scalar(daf, name, must_exist = TRUE)
```

## Arguments

- daf:

  A `DafReader`.

- name:

  Scalar name.

- must_exist:

  See `delete_axis`.

## Value

Invisibly the input `daf`.

## Examples

``` r
# Mirrors writers.jl jldoctest at line 109.
d <- example_cells_daf()
has_scalar(d, "organism")    # TRUE
#> [1] TRUE
delete_scalar(d, "organism")
has_scalar(d, "organism")    # FALSE
#> [1] FALSE
```
