# Create a chain of DafReaders with a final DafWriter.

Create a chain of DafReaders with a final DafWriter.

## Usage

``` r
chain_writer(dafs, name = NULL)
```

## Arguments

- dafs:

  Ordered list of `DafReader`s. Later entries override earlier entries
  on read.

- name:

  Optional chain name; defaults to `paste(names, collapse = ";")`.

## Value

A `WriteChainDaf`.

## Examples

``` r
base <- memory_daf(name = "base")
add_axis(base, "cell", c("c1", "c2"))
writable <- memory_daf(name = "writable")
ch <- chain_writer(list(base, writable))
set_scalar(ch, "organism", "human")
get_scalar(ch, "organism")
#> [1] "human"
```
