# Pack a FilesDaf directory's JSON metadata into `metadata.zip`.

Walks `path` and bundles `daf.json`, `axes/metadata.json`, every
`scalars/*.json`, every `vectors/<axis>/*.json`, and every
`matrices/<rows>/<cols>/*.json` into a single `path/metadata.zip`
archive, written atomically via `metadata.zip.new` + rename. Required
for serving a FilesDaf over HTTP via
[`http_daf()`](https://tanaylab.github.io/dafr/reference/HttpDaf.md);
from dafr 0.2.0 onward, FilesDaf writes maintain the bundle
automatically — call this only to repack a tree that was built by an
older dafr (pre-0.2.0) or modified outside dafr.

## Usage

``` r
pack_files_daf_metadata(path)
```

## Arguments

- path:

  Directory path to a FilesDaf root.

## Value

The absolute path to the written `metadata.zip`, invisibly.

## Examples

``` r
p <- tempfile("daf-")
files_daf(p, "w+")
#> <dafr::FilesDaf>
#>  @ name                  : chr "daf-1b6e42b8079d"
#>  @ internal              :<environment: 0x55c14a46f1f0> 
#>  @ cache                 :<environment: 0x55c14a46bee8> 
#>  @ axis_version_counter  :<environment: 0x55c14a46da78> 
#>  @ vector_version_counter:<environment: 0x55c14a46dd50> 
#>  @ matrix_version_counter:<environment: 0x55c14a46a1f8> 
pack_files_daf_metadata(p)
```
