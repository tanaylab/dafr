# Physically store the transposed layout of a matrix.

After this call, `get_matrix(columns_axis, rows_axis, name)` skips the
transpose-on-the-fly path.

## Usage

``` r
relayout_matrix(daf, rows_axis, columns_axis, name)
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

Invisibly the input `daf`.

## Examples

``` r
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2"))
add_axis(d, "gene", c("g1", "g2", "g3"))
m <- matrix(1:6, nrow = 2, ncol = 3)
set_matrix(d, "cell", "gene", "counts", m)
relayout_matrix(d, "cell", "gene", "counts")
has_matrix(d, "gene", "cell", "counts")
#> [1] TRUE
```
