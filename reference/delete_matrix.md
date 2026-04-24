# Delete a matrix.

Delete a matrix.

## Usage

``` r
delete_matrix(daf, rows_axis, columns_axis, name, must_exist = TRUE)
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

- must_exist:

  See `delete_axis`.

## Value

Invisibly the input `daf`.

## Examples

``` r
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2"))
add_axis(d, "gene", c("g1", "g2", "g3"))
m <- matrix(0L, nrow = 2, ncol = 3)
set_matrix(d, "cell", "gene", "counts", m)
delete_matrix(d, "cell", "gene", "counts")
has_matrix(d, "cell", "gene", "counts")
#> [1] FALSE
```
