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
# Mirrors readers.jl jldoctest at line 933.
m <- get_matrix(example_metacells_daf(), "gene", "metacell", "fraction")
dim(m)            # 683  7
#> [1] 683   7
colnames(m)       # 7 metacell IDs
#> [1] "M1671.28" "M2357.20" "M2169.56" "M2576.86" "M1440.15" "M756.63"  "M412.08" 
m[1:3, 1:2]
#>           M1671.28     M2357.20
#> RPL22 4.476663e-03 0.0041285986
#> PARK7 8.523007e-05 0.0001541991
#> ENO1  4.644479e-04 0.0004826086
```
