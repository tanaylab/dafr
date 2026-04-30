# Zarr-backed Daf class.

Concrete `DafWriter` subclass instantiated by
[`zarr_daf()`](https://tanaylab.github.io/dafr/reference/zarr_daf.md)
when opened with mode `"w"` / `"w+"` / `"r+"`. Operates against any
[ZarrStore](https://tanaylab.github.io/dafr/reference/ZarrStore.md);
today: `DirStore` (filesystem) and `DictStore` (in-memory). Zip-backed
Zarr (`MmapZipStore`) lands in slice 17.

## Usage

``` r
ZarrDaf(
  name = character(0),
  internal = new.env(parent = emptyenv()),
  cache = new.env(parent = emptyenv()),
  axis_version_counter = new.env(parent = emptyenv()),
  vector_version_counter = new.env(parent = emptyenv()),
  matrix_version_counter = new.env(parent = emptyenv()),
  store = ZarrStore()
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

- store:

  Internal
  [ZarrStore](https://tanaylab.github.io/dafr/reference/ZarrStore.md)
  instance (set by
  [`zarr_daf()`](https://tanaylab.github.io/dafr/reference/zarr_daf.md));
  not intended for direct use.

## Examples

``` r
tmp <- tempfile(fileext = ".daf.zarr")
d <- zarr_daf(tmp, mode = "w")
add_axis(d, "cell", c("c1", "c2"))
```
