# Open a daf storage path in a given mode.

Dispatches on path extension. Directory paths open a `FilesDaf`; paths
ending in `.h5df` or containing `.h5dfs#<group>` are reserved for an
H5df backend (not implemented).

## Usage

``` r
open_daf(path, mode = "r", name = NULL)
```

## Arguments

- path:

  Filesystem path.

- mode:

  One of `"r"` (read-only) or `"r+"` (read-write).

- name:

  Optional daf name. Default derived from the path basename.

## Value

A `DafReader` or `DafWriter`.

## Examples

``` r
tmp <- tempfile(); dir.create(tmp)
files_daf(tmp, name = "tmp", mode = "w+")
#> <dafr::FilesDaf>
#>  @ name                  : chr "tmp"
#>  @ internal              :<environment: 0x55e711ff4050> 
#>  @ cache                 :<environment: 0x55e711ff4be8> 
#>  @ axis_version_counter  :<environment: 0x55e70ff6ed00> 
#>  @ vector_version_counter:<environment: 0x55e70ff6efd8> 
#>  @ matrix_version_counter:<environment: 0x55e70ff6f2b0> 
d <- open_daf(tmp, "r")
```
