# Open a Zarr-backed Daf store.

Path-aware constructor that picks the right backing store. A path ending
in `.daf.zarr` (or any directory path) creates a `DirStore`. `:memory:`
or `NULL` creates a `DictStore`. A path ending in `.daf.zarr.zip`
creates an `MmapZipStore` (mmap-backed zip).

## Usage

``` r
zarr_daf(
  uri = NULL,
  mode = c("r", "r+", "w", "w+"),
  name = NULL,
  packed = FALSE
)
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

- packed:

  If `TRUE` (write modes only), dense vectors, dense matrices, and
  sparse-component arrays that exceed the packed-write size threshold
  are written as sharded (compressed) Zarr v3 arrays instead of flat;
  scalars and axes always stay flat. Per-component and threshold-gated,
  so a store may mix flat and packed components. Tunable via
  `options(dafr.packed_compression=, dafr.packed_compression_level=, dafr.packed_target_chunk_kb=)`.

## Value

A `ZarrDaf` (writeable) or `ZarrDafReadOnly`.

## Examples

``` r
tmp <- tempfile(fileext = ".daf.zarr")
d <- zarr_daf(tmp, mode = "w")
add_axis(d, "cell", c("c1", "c2"))
```
