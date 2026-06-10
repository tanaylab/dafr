# Read-only HTTP-served Zarr v3 store.

Implements the
[ZarrStore](https://tanaylab.github.io/dafr/reference/ZarrStore.md)
interface over HTTP(S). Used by
[`zarr_daf()`](https://tanaylab.github.io/dafr/reference/zarr_daf.md)
when given an `http(s)://` URL pointing at a `.daf.zarr` directory
served by a static web server. Reads the **Zarr v3** inline consolidated
metadata from the root `zarr.json` once at open: v3 stores carry
`consolidated_metadata.metadata` (a dict keyed by node path) in the root
group instead of the v2 `.zmetadata` file. Subsequent `store_exists` and
`store_list` resolve against that index, node `zarr.json` reads are
served from it (re-serialized to the bytes the reader would otherwise
have fetched per node), and chunk fetches are cached in process memory.

## Usage

``` r
HttpStore(
  url = character(0),
  index = list(),
  chunk_cache = new.env(parent = emptyenv())
)
```

## Arguments

- url:

  Root URL of the served Zarr v3 directory.

- index:

  Named list: the v3 node metadata index parsed from the root
  `zarr.json` `consolidated_metadata.metadata` (keyed by node path, with
  no `/zarr.json` suffix).

- chunk_cache:

  Environment caching fetched chunk bytes (and the root `zarr.json`) by
  path.

## Details

Writes hard-error: HTTP-served Zarr is read-only.

Zarr version: this backend targets the **Zarr v3** on-disk layout that
`dafr` reads and writes locally (DataAxesFormats.jl 0.3.0). A legacy v2
store (root `.zmetadata`, no root `zarr.json`) is rejected on open with
a `python -m zarr v2_to_v3` conversion hint, mirroring the local
`DirStore` / `MmapZipStore` v2 rejection. Note this is distinct from
[`http_daf()`](https://tanaylab.github.io/dafr/reference/HttpDaf.md)
(the FilesFormat-over-HTTP path in
[`http_daf()`](https://tanaylab.github.io/dafr/reference/HttpDaf.md)),
which uses `metadata.zip` and is unaffected.
