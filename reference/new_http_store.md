# Construct a read-only [ZarrStore](https://tanaylab.github.io/dafr/reference/ZarrStore.md) over HTTP (Zarr v3).

GETs the root `zarr.json` and parses its inline
`consolidated_metadata.metadata` as the node index, so it targets the
**Zarr v3** layout that `dafr` writes. A legacy v2 store (root
`.zmetadata`, no `zarr.json`) is rejected with a
`python -m zarr v2_to_v3` conversion hint; a v3 store that lacks inline
consolidated metadata cannot be enumerated over HTTP and is likewise
rejected. See
[HttpStore](https://tanaylab.github.io/dafr/reference/HttpStore.md) for
the supported alternatives.

## Usage

``` r
new_http_store(url)
```

## Arguments

- url:

  Root URL of a Zarr v3 directory served over HTTP(S).

## Value

An `HttpStore`.
