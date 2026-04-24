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
d <- memory_daf()
set_scalar(d, "organism", "human")
has_scalar(d, "organism")
#> [1] TRUE
has_scalar(d, "reference")
#> [1] FALSE
```
