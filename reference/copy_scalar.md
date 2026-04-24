# Copy a scalar from one daf to another.

Mirrors Julia
`copy_scalar!(; destination, source, name, rename, type, default, overwrite, insist)`.

## Usage

``` r
copy_scalar(
  destination,
  source,
  name,
  rename = NULL,
  type = NULL,
  default = .DAFR_UNDEF,
  overwrite = FALSE,
  insist = TRUE
)
```

## Arguments

- destination:

  A `DafWriter`.

- source:

  A `DafReader`.

- name:

  Name of the scalar in `source`.

- rename:

  If non-NULL, store under this name in `destination`.

- type:

  If non-NULL, coerce to this R storage type string (`"logical"`,
  `"integer"`, `"double"`, `"numeric"`, `"character"`).

- default:

  If unspecified, missing source raises. If `NULL`, missing source
  silently skips. Else, the value is used when source is absent.

- overwrite:

  If `TRUE`, replace an existing destination scalar.

- insist:

  If `TRUE` (default) and the destination already has the scalar, raise;
  if `FALSE`, silently skip.

## Value

Invisibly, the destination.

## Examples

``` r
src <- memory_daf(name = "src")
dest <- memory_daf(name = "dest")
set_scalar(src, "organism", "human")
copy_scalar(dest, src, "organism", rename = "species")
get_scalar(dest, "species")
#> [1] "human"
```
