# Read-only chain of DafReaders.

Produced by
[`chain_reader()`](https://tanaylab.github.io/dafr/reference/chain_reader.md).
Every `format_*` read falls through the chain in reverse order (last
wins); writes raise.

## Usage

``` r
ReadOnlyChainDaf(
  name = character(0),
  internal = new.env(parent = emptyenv()),
  cache = new.env(parent = emptyenv()),
  axis_version_counter = new.env(parent = emptyenv()),
  vector_version_counter = new.env(parent = emptyenv()),
  matrix_version_counter = new.env(parent = emptyenv()),
  dafs = list()
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

## Examples

``` r
base <- memory_daf(name = "base")
overlay <- memory_daf(name = "overlay")
ch <- chain_reader(list(base, overlay))
inherits(ch, "dafr::ReadOnlyChainDaf")
#> [1] TRUE
```
