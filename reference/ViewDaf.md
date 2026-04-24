# A read-only view over a base daf.

A ViewDaf carries a reference to a base `DafReader` and a dictionary of
per-name query overrides (axes / scalars / vectors / matrices). Reads on
the view rewrite into queries against the base; no data is copied.

## Usage

``` r
ViewDaf(
  name = character(0),
  internal = new.env(parent = emptyenv()),
  cache = new.env(parent = emptyenv()),
  axis_version_counter = new.env(parent = emptyenv()),
  vector_version_counter = new.env(parent = emptyenv()),
  matrix_version_counter = new.env(parent = emptyenv()),
  base = DafReader(),
  view_axes = list(),
  view_axis_renames = list(),
  view_axis_indices = list(),
  view_scalars = list(),
  view_vectors = list(),
  view_matrices = list()
)
```

## Arguments

- name:

  Character scalar; name of the view.

- internal:

  Internal environment (created by `new_internal_env()`).

- cache:

  Cache environment (created by `new_cache_env()`).

- axis_version_counter:

  Counter environment for axis version tracking.

- vector_version_counter:

  Counter environment for vector version tracking.

- matrix_version_counter:

  Counter environment for matrix version tracking.

- base:

  Base `DafReader` this view wraps.

- view_axes:

  Named list mapping view axis names to query strings.

- view_axis_renames:

  Named list mapping view axis names to base axis names.

- view_axis_indices:

  Named list mapping view axis names to 1-based integer vectors of
  positions within each base axis that the view exposes.

- view_scalars:

  Named list mapping view scalar names to query strings.

- view_vectors:

  Named list mapping `"axis|name"` keys to override specs.

- view_matrices:

  Named list mapping `"rows|cols|name"` keys to override specs.

## Details

Users should construct `ViewDaf` objects via
[`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md) rather
than calling the constructor directly.

## See also

[`viewer()`](https://tanaylab.github.io/dafr/reference/viewer.md)

## Examples

``` r
d <- example_cells_daf()
v <- viewer(d)
inherits(v, "dafr::ViewDaf")
#> [1] TRUE
```
