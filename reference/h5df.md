# Single-file (HDF5) Daf store.

A `Daf` store held in one `.h5df` HDF5 file, interoperable with Julia's
`DataAxesFormats.H5df`. The file holds a `daf` marker dataset plus
`scalars`/`axes`/`vectors`/`matrices` groups of typed HDF5 datasets.
Requires the `hdf5r` package.

## Usage

``` r
H5df(
  name = character(0),
  internal = new.env(parent = emptyenv()),
  cache = new.env(parent = emptyenv()),
  axis_version_counter = new.env(parent = emptyenv()),
  vector_version_counter = new.env(parent = emptyenv()),
  matrix_version_counter = new.env(parent = emptyenv())
)

H5dfReadOnly(
  name = character(0),
  internal = new.env(parent = emptyenv()),
  cache = new.env(parent = emptyenv()),
  axis_version_counter = new.env(parent = emptyenv()),
  vector_version_counter = new.env(parent = emptyenv()),
  matrix_version_counter = new.env(parent = emptyenv())
)

h5df(path, mode = c("r", "r+", "w", "w+"), name = NULL)
```

## Arguments

- name:

  Human-readable identifier. Default derived from the store's `name`
  scalar if present, else `basename(path)`.

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

- path:

  Path to a `.h5df` file.

- mode:

  One of `"r"` (read; must exist), `"r+"` (append; must exist), `"w"`
  (create; fails if it is already a daf store), `"w+"` (create or
  append).

## Value

An `H5df` (writable modes) or `H5dfReadOnly` (`"r"`).

## Examples

``` r
if (requireNamespace("hdf5r", quietly = TRUE)) {
  path <- tempfile("dafr-", fileext = ".h5df")
  d <- h5df(path, mode = "w")
  add_axis(d, "cell", c("c1", "c2"))
  set_scalar(d, "organism", "human")
  rm(d)
  unlink(path)
}
```
