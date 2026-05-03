# Read-only HTTP-served FilesDaf.

A `Daf` reader that fetches a
[`files_daf()`](https://tanaylab.github.io/dafr/reference/files_daf.md)
directory served over HTTP(S). The server must expose the FilesDaf tree
verbatim and include a `metadata.zip` bundle at the root (see
[`pack_files_daf_metadata()`](https://tanaylab.github.io/dafr/reference/pack_files_daf_metadata.md)).
From dafr 0.2.0 onward, FilesDaf writes maintain `metadata.zip`
automatically; for pre-0.2.0 stores call
[`pack_files_daf_metadata()`](https://tanaylab.github.io/dafr/reference/pack_files_daf_metadata.md)
before publishing.

## Usage

``` r
HttpDaf(
  name = character(0),
  internal = new.env(parent = emptyenv()),
  cache = new.env(parent = emptyenv()),
  axis_version_counter = new.env(parent = emptyenv()),
  vector_version_counter = new.env(parent = emptyenv()),
  matrix_version_counter = new.env(parent = emptyenv())
)

http_daf(url, name = NULL)
```

## Arguments

- name:

  Optional override; defaults to the daf's `name` scalar (if any) or the
  URL basename.

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

- url:

  HTTP(S) URL pointing at a FilesDaf root.

## Value

A `HttpDaf` instance (`DafReadOnly` subclass).

## Details

The client downloads `metadata.zip` once at open time, parses it in
memory, and serves all JSON metadata from it (no further HTTP traffic
for `format_has_*` / `format_*_set` / scalar reads). Non-JSON payloads
(axis `.txt` files, vector/matrix `.data` / `.nzind` / `.nzval` /
`.colptr` / `.rowval` / `.nztxt`) are fetched lazily on first access via
one HTTP GET each, cached by the standard cache layer.

Read-only — mutations are not supported. Server data is assumed stable
while a HttpDaf is open; reopen to pick up changes.

## HTTP timeout

Each GET uses a 30-second timeout. Override via
`options(dafr.http_timeout = N)` (seconds) or environment variable
`DAFR_HTTP_TIMEOUT` for slow links or large lazy payloads. Requests are
issued without retry; flaky-network handling is on the caller.

## URL-derived name

When neither `name` nor a `name` scalar in the daf is set, the default
name is `basename(url)`. Two HttpDafs opened from different hosts but
the same basename will collide on name; pass an explicit `name` to
disambiguate.

## Examples

``` r
if (FALSE) { # \dontrun{
d <- http_daf("https://example.com/path/to/data.daf/")
get_scalar(d, "organism")
} # }
```
