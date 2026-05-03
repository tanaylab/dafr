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
is_leaf(daf, ...)
```

## Arguments

- daf:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md).

- ...:

  Reserved for method-specific extensions.

## Value

Logical scalar.

## Examples

``` r
is_leaf(memory_daf())
#> [1] TRUE
```
