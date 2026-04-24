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
d <- memory_daf()
set_scalar(d, "organism", "human")
get_scalar(d, "organism")
#> [1] "human"
get_scalar(d, "reference", default = "GRCh38")
#> [1] "GRCh38"
```
