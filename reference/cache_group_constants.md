# Cache-group constants.

Returned by `format_get_*` / `format_set_*` backend methods to classify
how a value should be cached. Three constants:

## Usage

``` r
MEMORY_DATA

MAPPED_DATA

QUERY_DATA
```

## Format

An object of class `character` of length 1.

An object of class `character` of length 1.

An object of class `character` of length 1.

## Value

Character scalar.

## Details

- `MEMORY_DATA` — value is an in-memory R object; cache at the
  `"memory"` tier (subject to LRU + memory cap).

- `MAPPED_DATA` — value is an mmap-backed view (zero-copy); cache at the
  `"mapped"` tier (uncapped, no LRU).

- `QUERY_DATA` — value is a query-evaluation intermediate; cache at the
  `"query"` tier (subject to LRU + memory cap).

These names mirror upstream `DataAxesFormats.jl`'s `CacheGroup` enum.
The runtime tier names (`"mapped"` / `"memory"` / `"query"`) accept both
the constant form and the lowercase form via
[`empty_cache()`](https://tanaylab.github.io/dafr/reference/empty_cache.md).

## Examples

``` r
MEMORY_DATA
#> [1] "MemoryData"
MAPPED_DATA
#> [1] "MappedData"
QUERY_DATA
#> [1] "QueryData"
```
