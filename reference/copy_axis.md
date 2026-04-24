# Copy an axis (its entries) from one daf to another.

Mirrors Julia
`copy_axis!(; destination, source, axis, rename, overwrite, insist)`.

## Usage

``` r
copy_axis(
  destination,
  source,
  axis,
  rename = NULL,
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

- rename:

  If non-NULL, use this name in `destination`.

- overwrite:

  If `TRUE`, delete any existing destination axis (and all its
  properties) before recreating.

- insist:

  If `TRUE` (default) and the destination already has the axis, raise;
  if `FALSE`, silently skip.

## Value

Invisibly, the destination.

## Examples

``` r
src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1", "c2"))
dest <- memory_daf(name = "dest")
copy_axis(dest, src, "cell")
```
