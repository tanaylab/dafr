# Per-matrix version counter.

Returns the monotonic counter for the `name` matrix on
`(rows_axis, columns_axis)`. Mirrors Julia DAF: incremented every time
`set_matrix` is called (NOT on `delete_matrix` or `relayout_matrix`).
Returns `0L` if never set.

## Usage

``` r
matrix_version_counter(daf, rows_axis, columns_axis, name)
```

## Arguments

- daf:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md).

- rows_axis, columns_axis:

  Axis names.

- name:

  Matrix name.

## Value

`integer(1)`.

## Examples

``` r
# Mirrors readers.jl jldoctest at line 1088.
m <- example_metacells_daf()
matrix_version_counter(m, "gene", "metacell", "fraction") # 1L
#> [1] 1
set_matrix(m, "gene", "metacell", "fraction",
           matrix(stats::runif(683L * 7L), 683L, 7L),
           overwrite = TRUE)
matrix_version_counter(m, "gene", "metacell", "fraction") # 2L
#> [1] 2
```
