# Open a Daf store by URI or path.

Path/URL-aware factory that dispatches to the right backend. Today
supports `memory://` (or no path / NULL) for in-memory stores, regular
filesystem paths for `files_daf`, and `*.daf.zarr` directories for
`zarr_daf`. `*.daf.zarr.zip` errors with "lands in slice 17";
`http(s)://` errors with "lands in slice 18".

## Usage

``` r
open_daf(uri = NULL, mode = "r", name = NULL, ...)
```

## Arguments

- uri:

  Path or URL. `memory://` (or `NULL` / empty string) for an in-memory
  store; a filesystem directory path for `files_daf`; a URL with a
  recognized scheme for future backends.

- mode:

  One of `"r"`, `"r+"`, `"w"`, `"w+"`. Required for `files_daf`; ignored
  for `memory_daf`.

- name:

  Optional name for the daf object. Default derived from the URI.

- ...:

  Reserved for backend-specific options.

## Value

A `DafReader` or `DafWriter` (subclass depends on backend and mode).

## Examples

``` r
d <- open_daf("memory://", name = "demo")
add_axis(d, "cell", c("c1", "c2"))
```
