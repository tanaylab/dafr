# File-backed Daf store.

A `Daf` store backed by a directory of small self-describing files,
bidirectionally compatible with Julia's `DataAxesFormats.FilesDaf`.
Writes are non-atomic; only one writer may touch a store at a time.

## Usage

``` r
files_daf(path, mode = c("r", "r+", "w", "w+"), name = NULL, packed = FALSE)
```

## Arguments

- path:

  Directory path.

- mode:

  One of `"r"` (read-only, store must exist), `"r+"` (read-write, store
  must exist), `"w"` (create; fails if store already exists), `"w+"`
  (create or open an existing store; if the store already exists, its
  `scalars`, `axes`, `vectors`, `matrices` subdirectories and `daf.json`
  are truncated, but unrelated files in the directory are preserved).

- name:

  Human-readable identifier. Defaults to `basename(path)`.

- packed:

  When `TRUE` (writeable modes only), dense vectors, dense matrices, and
  large sparse components that exceed the packing threshold are written
  as compressed dual-format `.zip` shards (DataAxesFormats.jl 0.3.0
  "indexed+zipped" packed format) instead of flat binary payloads. Small
  components and string properties always stay flat. Tuning is read from
  the `dafr.packed_compression`, `dafr.packed_compression_level`, and
  `dafr.packed_target_chunk_kb` options. Defaults to `FALSE` (flat
  layout).

## Value

A `FilesDaf` instance (`DafWriter` under `"r+"`/`"w"`/`"w+"`,
`FilesDafReadOnly`/`DafReadOnly` under `"r"`).

## Concurrent access

`files_daf` does not lock the store. Two writers opening the same path
in mode `"r+"` or `"w+"` will race on `metadata.zip` rebuilds and
per-entry JSON writes, with no guarantee of last-writer-wins
consistency. The supported pattern is single-writer plus arbitrary
read-only readers; cross-process concurrency must be coordinated
externally (e.g., a job scheduler).

## Examples

``` r
path <- tempfile("dafr-example-")
d <- files_daf(path, mode = "w")
add_axis(d, "cell", c("c1", "c2"))
set_scalar(d, "organism", "human")
rm(d)
unlink(path, recursive = TRUE)
```
