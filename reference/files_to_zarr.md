# Convert a `files_daf` directory to a `zarr_daf` directory.

Both formats store identical raw little-endian numeric payloads on disk.
This conversion re-serializes only the JSON metadata and the string
blobs (which differ between formats: files-format uses newline-delimited
UTF-8, while Zarr uses `vlen-utf8`).

## Usage

``` r
files_to_zarr(src, dst)
```

## Arguments

- src:

  Path to an existing `files_daf` directory.

- dst:

  Path to a non-existing destination directory; will be created. Errors
  if `dst` already exists.

## Value

Invisibly the destination path.

## Details

Same-filesystem only. The function probes a hard-link from `src` to
`dst` up front; if the link fails (typically because the paths are on
different filesystems) the call errors with no automatic copy fallback.

Note: this implementation is correctness-first; the numeric blobs are
re-written through the regular `set_*` API rather than hard-linked. The
probe still enforces the same-filesystem constraint so a future
hard-link optimization can ship without an API break.

## Examples

``` r
if (FALSE) { # \dontrun{
files_to_zarr("/path/to/files.daf", "/path/to/converted.daf.zarr")
} # }
```
