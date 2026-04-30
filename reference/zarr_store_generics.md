# Zarr v2 store generics.

Low-level byte-level operations on a
[ZarrStore](https://tanaylab.github.io/dafr/reference/ZarrStore.md). All
Zarr v2 I/O goes through these five generics so the storage backend is
interchangeable.

## Usage

``` r
store_get_bytes(store, path, ...)

store_set_bytes(store, path, bytes, ...)

store_delete(store, path, ...)

store_exists(store, path, ...)

store_list(store, prefix, ...)
```

## Arguments

- store:

  A [ZarrStore](https://tanaylab.github.io/dafr/reference/ZarrStore.md).

- path:

  Character scalar key (slash-separated path string).

- ...:

  Not used; present for S7 generic dispatch compatibility.

- bytes:

  Raw vector of bytes to write.

- prefix:

  Character scalar; `""` to list all keys.

## Value

See individual descriptions above.

## Details

- `store_get_bytes(store, path)` — return raw vector, or `NULL` if
  missing.

- `store_set_bytes(store, path, bytes)` — write raw vector; create dirs.

- `store_delete(store, path)` — remove key; silent no-op if absent.

- `store_exists(store, path)` — logical scalar.

- `store_list(store, prefix)` — character vector of keys with given
  prefix; `prefix = ""` lists all keys.
