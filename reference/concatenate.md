# Concatenate multiple dafs along one or more axes.

Mirrors Julia `concatenate!()`. For each concatenation axis, entries
from each source are appended in source order. Non-concat axes must be
identical across all sources and are copied once.

## Usage

``` r
concatenate(
  destination,
  axis,
  sources,
  names = NULL,
  dataset_axis = "dataset",
  dataset_property = TRUE,
  prefix = FALSE,
  prefixed = NULL,
  empty = NULL,
  merge = NULL,
  sparse_if_saves_storage_fraction = 0.25,
  overwrite = FALSE
)
```

## Arguments

- destination:

  A `DafWriter`. Must be empty of the concatenation axes.

- axis:

  A single axis name or a character vector of axis names.

- sources:

  List of `DafReader`s to concatenate.

- names:

  Optional character vector of unique data set names (defaults to each
  source's `name` prop).

- dataset_axis:

  Name of the per-source axis to create. `NULL` disables.

- dataset_property:

  If `TRUE` (default) and `dataset_axis` is non-NULL, create a
  same-named vector on every concatenation axis holding the source name
  for each entry.

- prefix:

  Logical (single or per-axis). Prefix concat-axis entries with
  `"<dataset_name>."` to de-duplicate across sources.

- prefixed:

  Optional character vector (or list of vectors per axis) of additional
  property names to prefix, beyond the heuristic (same-name or
  `"<axis>.*"` properties).

- empty:

  Named list of fill values for missing per-source properties.

- merge:

  Named list mapping property keys to a merge action (`"SkipProperty"`,
  `"LastValue"`, `"CollectAxis"`).

- sparse_if_saves_storage_fraction:

  Numeric (default 0.25); reserved for a future sparse-promotion
  heuristic.

- overwrite:

  If `TRUE`, allow replacing pre-existing destination entries.

## Value

Invisibly, the destination.

## Examples

``` r
a <- memory_daf(name = "A"); add_axis(a, "cell", c("a1", "a2"))
b <- memory_daf(name = "B"); add_axis(b, "cell", c("b1"))
dest <- memory_daf(name = "dest")
concatenate(dest, "cell", list(a, b))
```
