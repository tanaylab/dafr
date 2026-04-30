# Open a Zarr-backed Daf store.

Path-aware constructor that picks the right backing store. A path ending
in `.daf.zarr` (or any directory path) creates a `DirStore`. `:memory:`
or `NULL` creates a `DictStore`. `*.daf.zarr.zip` errors with "lands in
slice 17" pending the C++ MmapZipStore.

## Usage

``` r
zarr_daf(uri = NULL, mode = c("r", "r+", "w", "w+"), name = NULL)
```

## Arguments

- uri:

  Filesystem directory path, `:memory:`, or `NULL`.

- mode:

  `"r"`, `"r+"`, `"w"`, or `"w+"`. `"w"` initializes a fresh empty store
  (creating the directory if needed). `"w+"` truncates any existing
  store and creates a fresh one. `"r+"` opens for read-write without
  truncating. `"r"` opens read-only.

- name:

  Optional name; default derived from `uri`.

## Value

A `ZarrDaf` (writeable) or `ZarrDafReadOnly`.

## Examples

``` r
tmp <- tempfile(fileext = ".daf.zarr")
d <- zarr_daf(tmp, mode = "w")
add_axis(d, "cell", c("c1", "c2"))
```
