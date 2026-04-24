# Write chain of DafReaders with a final DafWriter.

Produced by
[`chain_writer()`](https://tanaylab.github.io/dafr/reference/chain_writer.md).
Reads fall through in reverse order (writer last-wins); writes go to the
final writer; deletes succeed only if the entry does not exist in any
earlier daf.

## Usage

``` r
WriteChainDaf(
  name = character(0),
  internal = new.env(parent = emptyenv()),
  cache = new.env(parent = emptyenv()),
  axis_version_counter = new.env(parent = emptyenv()),
  vector_version_counter = new.env(parent = emptyenv()),
  matrix_version_counter = new.env(parent = emptyenv()),
  dafs = list(),
  writer = DafWriter()
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

- dafs:

  Ordered list of base `DafReader`s.

- writer:

  The final `DafWriter` (== `dafs[[length(dafs)]]`).

## Examples

``` r
base <- memory_daf(name = "base")
writer <- memory_daf(name = "writer")
ch <- chain_writer(list(base, writer))
inherits(ch, "dafr::WriteChainDaf")
#> [1] TRUE
```
