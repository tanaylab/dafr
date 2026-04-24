# Copy a matrix from one daf to another.

Mirrors Julia
`copy_matrix!(; destination, source, rows_axis, columns_axis, name, rows_reaxis, columns_reaxis, rename, eltype, default, empty, relayout, overwrite, insist)`.

## Usage

``` r
copy_matrix(
  destination,
  source,
  rows_axis,
  columns_axis,
  name,
  rows_reaxis = NULL,
  columns_reaxis = NULL,
  rename = NULL,
  type = NULL,
  default = .DAFR_UNDEF,
  empty = NULL,
  relayout = TRUE,
  overwrite = FALSE,
  insist = TRUE
)
```

## Arguments

- destination:

  A `DafWriter`.

- source:

  A `DafReader`.

- rows_axis, columns_axis:

  Axis names in `source`.

- name:

  Matrix name in `source`.

- rows_reaxis, columns_reaxis:

  If non-NULL, store on these destination axes (axes must already exist
  in `destination`).

- rename:

  If non-NULL, store under this name.

- type:

  If non-NULL, coerce to this storage type string.

- default:

  If unspecified, missing source raises. If `NULL`, silently skips. Else
  scalar filled into full source-shape matrix.

- empty:

  Value filled for entries whose row or column is missing in source but
  present in destination.

- relayout:

  If `TRUE` (default), also write the transposed layout.

- overwrite, insist:

  See
  [`copy_scalar()`](https://tanaylab.github.io/dafr/reference/copy_scalar.md).

## Value

Invisibly, the destination.

## Examples

``` r
src <- memory_daf(name = "src")
add_axis(src, "cell", c("c1", "c2"))
add_axis(src, "gene", c("g1", "g2"))
set_matrix(src, "cell", "gene", "UMIs",
           matrix(1:4, nrow = 2,
                  dimnames = list(c("c1","c2"), c("g1","g2"))))
dest <- memory_daf(name = "dest")
add_axis(dest, "cell", c("c1", "c2"))
add_axis(dest, "gene", c("g1", "g2"))
copy_matrix(dest, src, "cell", "gene", "UMIs", relayout = FALSE)
```
