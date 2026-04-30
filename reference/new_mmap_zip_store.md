# Stub for the mmap-backed zip store (slice 17).

Errors with a message pointing to slice 17. The constructor exists so
future routing in `open_daf` can dispatch on `*.daf.zarr.zip` URIs
without callers having to special-case the absence of the
implementation.

## Usage

``` r
new_mmap_zip_store(path)
```

## Arguments

- path:

  Filesystem path to a zip archive.

## Value

Throws.

## Examples

``` r
if (FALSE) { # \dontrun{
new_mmap_zip_store("/path/to/foo.daf.zarr.zip")
} # }
```
