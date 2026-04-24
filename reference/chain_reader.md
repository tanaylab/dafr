# Create a read-only chain of DafReaders.

Create a read-only chain of DafReaders.

## Usage

``` r
chain_reader(dafs, name = NULL)
```

## Arguments

- dafs:

  Ordered list of `DafReader`s. Later entries override earlier entries
  on read.

- name:

  Optional chain name; defaults to `paste(names, collapse = ";")`.

## Value

A `ReadOnlyChainDaf`.

## Examples

``` r
base <- memory_daf(name = "base")
add_axis(base, "cell", c("c1", "c2"))
set_scalar(base, "organism", "human")
overlay <- memory_daf(name = "overlay")
set_scalar(overlay, "reference", "GRCh38")
ch <- chain_reader(list(base, overlay))
scalars_set(ch)
#> [1] "organism"  "reference"
```
