# Wrap a writer into a read-only view via a 1-element chain.

Returns a `ReadOnlyChainDaf` that reads from `daf` but rejects writes.
Implementation delegates to
[`chain_reader()`](https://tanaylab.github.io/dafr/reference/chain_reader.md)
with a single entry; there is no separate read-only class.

## Usage

``` r
read_only(daf, name = NULL)
```

## Arguments

- daf:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md)
  (typically a
  [DafWriter](https://tanaylab.github.io/dafr/reference/DafWriter.md)).

- name:

  Optional chain name; defaults to `daf_name(daf)`.

## Value

A
[ReadOnlyChainDaf](https://tanaylab.github.io/dafr/reference/ReadOnlyChainDaf.md).

## See also

[`chain_reader()`](https://tanaylab.github.io/dafr/reference/chain_reader.md),
[`is_daf()`](https://tanaylab.github.io/dafr/reference/is_daf.md)

## Examples

``` r
d <- memory_daf(name = "inner")
add_axis(d, "cell", c("c1", "c2"))
ro <- read_only(d)
daf_name(ro)
#> [1] "inner"
```
