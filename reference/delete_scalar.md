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
d <- memory_daf()
set_scalar(d, "organism", "human")
delete_scalar(d, "organism")
has_scalar(d, "organism")
#> [1] FALSE
```
