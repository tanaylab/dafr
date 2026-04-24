# In-memory Daf store.

A concrete `DafWriter` backed entirely by R environments — no disk, no
mmap. Scalars, axes, vectors, and matrices live in nested environments
(hash tables) under the `internal` property:

## Usage

``` r
memory_daf(name = "memory")
```

## Arguments

- name:

  Human-readable identifier. Defaults to `"memory"`.

## Value

A `MemoryDaf` instance.

## Details

- `internal$scalars` : `env(name -> value)`

- `internal$axes` : `env(axis -> list(entries = character, dict = env))`

- `internal$vectors` : `env(axis -> env(name -> vector))`

- `internal$matrices` :
  `env(rows_axis -> env(columns_axis -> env(name -> matrix)))`

## Examples

``` r
d <- memory_daf(name = "scratch")
add_axis(d, "cell", c("A", "B", "C"))
set_vector(d, "cell", "donor", c("d1", "d2", "d1"))
```
