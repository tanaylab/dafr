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
d <- memory_daf()
set_scalar(d, "organism", "human")
get_scalar(d, "organism")
#> [1] "human"
```
