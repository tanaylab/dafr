# Convert a `zarr_daf` directory to a `files_daf` directory.

Reverse of
[`files_to_zarr()`](https://tanaylab.github.io/dafr/reference/files_to_zarr.md).
Re-serializes JSON metadata and string blobs. Same-filesystem only -
cross-filesystem hard-links fail and produce an error (no automatic copy
fallback).

## Usage

``` r
zarr_to_files(src, dst)
```

## Arguments

- src:

  Path to an existing `files_daf` directory.

- dst:

  Path to a non-existing destination directory; will be created. Errors
  if `dst` already exists.

## Value

Invisibly the destination path.

## Examples

``` r
if (FALSE) { # \dontrun{
zarr_to_files("/path/to/source.daf.zarr", "/path/to/converted.daf")
} # }
```
