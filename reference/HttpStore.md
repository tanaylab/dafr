# Read-only HTTP-served Zarr store.

Implements the
[ZarrStore](https://tanaylab.github.io/dafr/reference/ZarrStore.md)
interface over HTTP(S). Used by
[`zarr_daf()`](https://tanaylab.github.io/dafr/reference/zarr_daf.md)
when given an `http(s)://` URL pointing at a `.daf.zarr` directory
served by a static web server. Reads `.zmetadata` (consolidated
metadata) once at open; subsequent `store_exists` / `store_list` resolve
against the parsed dictionary, and chunk fetches are cached in process
memory.

## Usage

``` r
HttpStore(
  url = character(0),
  zmetadata = list(),
  chunk_cache = new.env(parent = emptyenv())
)
```

## Arguments

- url:

  Root URL of the served Zarr v2 directory.

- zmetadata:

  Parsed consolidated `.zmetadata` content.

- chunk_cache:

  Environment caching fetched chunk bytes by path.

## Details

Writes hard-error: HTTP-served Zarr is read-only.
