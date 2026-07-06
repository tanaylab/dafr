# Rebuild a FilesDaf store's root metadata.json index.

Writes the DataAxesFormats-compatible `metadata.json` consolidated index
from the on-disk tree. Use to migrate a store written by an older dafr
(which used `metadata.zip`) or modified outside dafr, so it can be
served over HTTP and read by DataAxesFormats.jl.

## Usage

``` r
pack_files_daf_metadata(path)
```

## Arguments

- path:

  FilesDaf store root directory.

## Value

`path`, invisibly.
