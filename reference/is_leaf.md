# Is `daf` a leaf storage format?

Returns `TRUE` for storage classes that own their on-disk or in-memory
state directly (`MemoryDaf`, `FilesDaf` / `FilesDafReadOnly`, `ZarrDaf`
/ `ZarrDafReadOnly`, `HttpDaf`) and `FALSE` for wrappers
(`ReadOnlyChainDaf`, `WriteChainDaf`, `ContractDaf`, `ViewDaf`). Used by
[`reorder_axes()`](https://tanaylab.github.io/dafr/reference/reorder_axes.md)
to reject non-leaf inputs because permuting indices is only meaningful
on the underlying storage. Mirrors upstream Julia `Readers.is_leaf`.

## Usage

``` r
is_leaf(daf)
```

## Arguments

- daf:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md)
  instance or an S7 class object.

## Value

Logical scalar.

## Details

Accepts either a `DafReader` instance or an S7 class object (so that
`is_leaf(MemoryDaf)` mirrors Julia's class-level
`is_leaf(::Type{MemoryDaf})`).

## Examples

``` r
is_leaf(memory_daf())
#> [1] TRUE
is_leaf(MemoryDaf)
#> [1] TRUE
```
