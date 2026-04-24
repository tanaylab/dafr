# Set a matrix indexed by a pair of axes.

Set a matrix indexed by a pair of axes.

## Usage

``` r
set_matrix(daf, rows_axis, columns_axis, name, mat, overwrite = FALSE)
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

- mat:

  Dense `matrix`, or sparse `dgCMatrix` / `lgCMatrix`, of shape
  `axis_length(rows_axis) x axis_length(columns_axis)`.

- overwrite:

  See `set_scalar`.

## Value

Invisibly the input `daf`.

## Examples

``` r
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2"))
add_axis(d, "gene", c("g1", "g2", "g3"))
m <- matrix(1:6, nrow = 2, ncol = 3,
    dimnames = list(c("c1", "c2"), c("g1", "g2", "g3")))
set_matrix(d, "cell", "gene", "counts", m)
dim(get_matrix(d, "cell", "gene", "counts"))
#> [1] 2 3
```
