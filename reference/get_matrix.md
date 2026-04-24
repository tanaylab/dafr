# Get a matrix, returning it with axis-entry dimnames.

When the matrix is stored only at the flipped-layout axis pair
`(columns_axis, rows_axis)`, this function transposes on-the-fly and
returns with the requested dimnames.

## Usage

``` r
get_matrix(daf, rows_axis, columns_axis, name, default)
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

- default:

  If supplied and the matrix is absent under both layouts, return a
  constant-valued `nrow x ncol` matrix with axis entries as dimnames.

## Value

Dense `matrix` or sparse `dgCMatrix` / `lgCMatrix` with dimnames set.

## Examples

``` r
d <- example_cells_daf()
m <- get_matrix(d, "cell", "gene", "UMIs")
dim(m)
#> [1] 856 683
```
