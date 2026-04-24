# Copy everything from one daf to another.

Mirrors Julia
`copy_all!(; destination, source, empty, types, overwrite, insist, relayout)`.
Copies in order: scalars, axes, vectors, matrices. Tensors are not
auto-expanded here — users can call
[`copy_tensor()`](https://tanaylab.github.io/dafr/reference/copy_tensor.md)
explicitly for per-main-axis-entry matrix groups.

## Usage

``` r
copy_all(
  destination,
  source,
  empty = NULL,
  types = NULL,
  overwrite = FALSE,
  insist = TRUE,
  relayout = TRUE
)
```

## Arguments

- destination:

  A `DafWriter`.

- source:

  A `DafReader`.

- empty:

  Named list mapping flat keys to fill values: `"axis|name" -> value`
  for vectors, `"rows|cols|name" -> value` for matrices. Matrix keys
  accept either axis order.

- types:

  Named list of type-coercion strings in the same flat-key form plus
  `"name"` (no pipes) for scalars.

- overwrite:

  If `TRUE`, replace pre-existing destination entries.

- insist:

  If `TRUE` (default) raise on pre-existing conflicts when
  `overwrite = FALSE`; if `FALSE` silently skip.

- relayout:

  If `TRUE` (default), also write transposed layout for copied matrices.

## Value

Invisibly, the destination.

## Details

Axes already in the destination are not overwritten (regardless of the
`overwrite` flag). A destination axis must be identical to or a subset
of the source axis (else `empty` is required per-vector / per-matrix to
fill missing entries). Unknown-to-source destination axes are left
untouched.

## Examples

``` r
src <- memory_daf(name = "src")
set_scalar(src, "organism", "human")
add_axis(src, "cell", c("c1", "c2"))
set_vector(src, "cell", "age", c(10L, 20L))
dest <- memory_daf(name = "dest")
copy_all(dest, src, relayout = FALSE)
```
