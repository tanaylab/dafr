# Create an in-memory Zarr v2 store.

Backed by an R environment keyed by path strings. Useful for tests and
for ZarrDaf instances that don't need persistence.

## Usage

``` r
new_dict_store()
```

## Value

A `DictStore`.

## Examples

``` r
s <- new_dict_store()
```
