# Construct a read-only [ZarrStore](https://tanaylab.github.io/dafr/reference/ZarrStore.md) over HTTP.

Construct a read-only
[ZarrStore](https://tanaylab.github.io/dafr/reference/ZarrStore.md) over
HTTP.

## Usage

``` r
new_http_store(url)
```

## Arguments

- url:

  Root URL of a Zarr v2 directory served over HTTP(S).

## Value

An `HttpStore`.
