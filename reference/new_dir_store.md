# Create a directory-backed Zarr v2 store.

Each key maps to a regular file under `root`. Directories are created as
needed. The root must be writable.

## Usage

``` r
new_dir_store(root)
```

## Arguments

- root:

  Filesystem path; created if missing.

## Value

A `DirStore`.

## Examples

``` r
tmp <- tempfile()
s <- new_dir_store(tmp)
```
