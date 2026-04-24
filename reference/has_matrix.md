# Test whether a matrix exists for an axis pair.

Test whether a matrix exists for an axis pair.

## Usage

``` r
has_matrix(daf, rows_axis, columns_axis, name)
```

## Arguments

- daf:

  A `DafReader`.

- rows_axis:

  Row-axis name.

- columns_axis:

  Column-axis name.

- name:

  Matrix name.

## Value

Logical scalar.

## Examples

``` r
d <- example_cells_daf()
has_matrix(d, "gene", "cell", "UMIs")
#> [1] TRUE
has_matrix(d, "cell", "gene", "UMIs")
#> [1] TRUE
```
