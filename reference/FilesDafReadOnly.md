# File-backed read-only Daf class.

Concrete `DafReadOnly` subclass instantiated by
[`files_daf()`](https://tanaylab.github.io/dafr/reference/files_daf.md)
when opened with mode `"r"`. All mutating `format_*` generics reject
calls on this class with a clear "store opened read-only" error.

## Usage

``` r
FilesDafReadOnly(
  name = character(0),
  internal = new.env(parent = emptyenv()),
  cache = new.env(parent = emptyenv()),
  axis_version_counter = new.env(parent = emptyenv()),
  vector_version_counter = new.env(parent = emptyenv()),
  matrix_version_counter = new.env(parent = emptyenv())
)
```

## Arguments

- name:

  Human-readable identifier for the `Daf` store.

- internal:

  Internal per-store environment used by format backends to stash
  backend-specific state; reserved for package use.

- cache:

  Three-tier cache environment (mapped / memory / query). See
  `new_cache_env()`.

- axis_version_counter:

  Environment tracking per-axis mutation counters; invalidates cached
  reads when an axis is modified.

- vector_version_counter:

  Environment tracking per-vector mutation counters.

- matrix_version_counter:

  Environment tracking per-matrix mutation counters.

## Examples

``` r
if (FALSE) { # \dontrun{
d <- files_daf("/path/to/existing/store", mode = "r")
inherits(d, "dafr::FilesDafReadOnly")
} # }
```
