# File-backed Daf store.

A `Daf` store backed by a directory of small self-describing files,
bidirectionally compatible with Julia's `DataAxesFormats.FilesDaf`.
Writes are non-atomic; only one writer may touch a store at a time.

## Usage

``` r
files_daf(path, mode = c("r", "r+", "w", "w+"), name = NULL)
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

## Value

A `FilesDaf` instance (`DafWriter` under `"r+"`/`"w"`/`"w+"`,
`FilesDafReadOnly`/`DafReadOnly` under `"r"`).

## Examples

``` r
path <- tempfile("dafr-example-")
d <- files_daf(path, mode = "w")
add_axis(d, "cell", c("c1", "c2"))
set_scalar(d, "organism", "human")
rm(d)
unlink(path, recursive = TRUE)
```
