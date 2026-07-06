# Single-file (zip) Daf store.

A `Daf` store held in one append-only `.daf.zip` archive,
byte-compatible with Julia's `DataAxesFormats.ZipDaf`. Same on-disk
layout as
[`files_daf()`](https://tanaylab.github.io/dafr/reference/files_daf.md)
but inside a ZIP archive (whose central directory replaces
`metadata.json`). The archive is append-only: overwriting or deleting a
property, or reordering an axis, raises an error.

## Usage

``` r
zip_daf(path, mode = c("r", "r+", "w", "w+"), name = NULL, packed = FALSE)
```

## Arguments

- path:

  Path to a `.daf.zip` archive.

- mode:

  One of `"r"` (read; must exist), `"r+"` (append; must exist), `"w"`
  (create; fails if it is already a daf archive), `"w+"` (create or
  append).

- name:

  Human-readable identifier. Default derived from the archive's `name`
  scalar if present, else `basename(path)`.

- packed:

  When `TRUE` (writeable modes), large numeric components are written as
  packed `.zip` shards, as in
  [`files_daf()`](https://tanaylab.github.io/dafr/reference/files_daf.md).

## Value

A `ZipDaf` (writable modes) or `ZipDafReadOnly` (`"r"`).

## Examples

``` r
# ZipDaf is built on the POSIX-only MmapZipStore, so it is unavailable on
# Windows; guard the example accordingly.
if (.Platform$OS.type != "windows") {
  path <- tempfile("dafr-zip-", fileext = ".daf.zip")
  d <- zip_daf(path, mode = "w")
  add_axis(d, "cell", c("c1", "c2"))
  set_scalar(d, "organism", "human")
  rm(d)
  unlink(path)
}
```
