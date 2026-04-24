# Copy a vector from one daf to another.

Mirrors Julia
`copy_vector!(; destination, source, axis, name, reaxis, rename, eltype, default, empty, overwrite, insist)`.

## Usage

``` r
copy_vector(
  destination,
  source,
  axis,
  name,
  rename = NULL,
  reaxis = NULL,
  type = NULL,
  default = .DAFR_UNDEF,
  empty = NULL,
  overwrite = FALSE,
  insist = TRUE
)
```

## Arguments

- destination:

  A `DafWriter`.

- source:

  A `DafReader`.

- axis:

  Axis name in `source`.

- name:

  Vector name in `source`.

- rename:

  If non-NULL, store under this name in `destination`.

- reaxis:

  If non-NULL, store on this (already-existing) destination axis.

- type:

  If non-NULL, coerce to this storage type string.

- default:

  If unspecified, missing source raises. If `NULL`, missing source
  silently skips. Else, a scalar (filled into every entry) or vector
  used when source is absent.

- empty:

  Value filled for destination-axis entries not present in the source
  axis (required when source axis is a subset of destination).

- overwrite, insist:

  See
  [`copy_scalar()`](https://tanaylab.github.io/dafr/reference/copy_scalar.md).

## Value

Invisibly, the destination.

## Examples

``` r
src <- memory_daf(name = "src")
add_axis(src, "cell", c("c1", "c2"))
set_vector(src, "cell", "age", c(10L, 20L))
dest <- memory_daf(name = "dest")
add_axis(dest, "cell", c("c1", "c2"))
copy_vector(dest, src, "cell", "age")
```
