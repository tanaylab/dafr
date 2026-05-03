# Open a Daf store by URI or path.

Path/URL-aware factory that dispatches to the right backend. Supported
URIs:

- `memory://` (or no path / `NULL`) — in-memory
  [`memory_daf()`](https://tanaylab.github.io/dafr/reference/memory_daf.md).

- filesystem directory path —
  [`files_daf()`](https://tanaylab.github.io/dafr/reference/files_daf.md).

- `*.daf.zarr` or `*.daf.zarr.zip` (filesystem or HTTP) —
  [`zarr_daf()`](https://tanaylab.github.io/dafr/reference/zarr_daf.md).

- any other `http(s)://` URL —
  [`http_daf()`](https://tanaylab.github.io/dafr/reference/HttpDaf.md)
  (read-only HTTP-served FilesDaf).

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

## Details

HTTP backends are read-only; modes other than `"r"` are rejected. HTTP
zip-archive URLs are not supported (open the underlying `.daf.zarr`
directory instead).

## Examples

``` r
d <- open_daf("memory://", name = "demo")
add_axis(d, "cell", c("c1", "c2"))
```
