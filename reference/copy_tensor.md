# Copy a tensor (set of per-main-axis-entry matrices) between dafs.

Mirrors Julia
`copy_tensor!(; destination, source, main_axis, rows_axis, columns_axis, name, rows_reaxis, columns_reaxis, rename, eltype, empty, relayout, overwrite, insist)`.

## Usage

``` r
copy_tensor(
  destination,
  source,
  main_axis,
  rows_axis,
  columns_axis,
  name,
  rows_reaxis = NULL,
  columns_reaxis = NULL,
  rename = NULL,
  type = NULL,
  empty = NULL,
  relayout = TRUE,
  overwrite = FALSE,
  insist = TRUE
)
```

## Arguments

- destination, source:

  Daf data sets.

- main_axis:

  Axis whose entries define the per-matrix loop.

- rows_axis, columns_axis:

  Matrix row/column axes.

- name:

  Base name; full matrix name is `"<main_entry>_<name>"`.

- rows_reaxis, columns_reaxis, rename, type, empty, relayout, overwrite,
  insist:

  See
  [`copy_matrix()`](https://tanaylab.github.io/dafr/reference/copy_matrix.md).

## Value

Invisibly, the destination.

## Details

Iterates over `main_axis` entries in the destination. For each entry
`E`, copies the matrix named `"E_<name>"` (or `"E_<rename>"`) from
source to destination. If a per-entry source matrix is missing, `empty`
is used as the fill value. This supports a destination main axis that is
a strict superset of the source's.

## Examples

``` r
src <- memory_daf(name = "src")
add_axis(src, "batch", c("b1", "b2"))
add_axis(src, "gene", c("g1"))
add_axis(src, "cell", c("c1"))
set_matrix(src, "gene", "cell", "b1_counts",
           matrix(1, 1, 1, dimnames = list("g1", "c1")))
set_matrix(src, "gene", "cell", "b2_counts",
           matrix(2, 1, 1, dimnames = list("g1", "c1")))
dest <- memory_daf(name = "dest")
add_axis(dest, "batch", c("b1", "b2"))
add_axis(dest, "gene", c("g1"))
add_axis(dest, "cell", c("c1"))
copy_tensor(dest, src, "batch", "gene", "cell", "counts", relayout = FALSE)
```
