# Per-matrix version counter.

Returns the monotonic counter for the `name` matrix on
`(rows_axis, columns_axis)`. Incremented on `set_matrix` /
`delete_matrix` / `relayout_matrix`. Returns `0L` if never mutated.

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
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2"))
add_axis(d, "gene", c("g1", "g2", "g3"))
matrix_version_counter(d, "cell", "gene", "UMIs") # 0L
#> [1] 0
```
