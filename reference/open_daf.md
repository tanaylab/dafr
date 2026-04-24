# Open a daf storage path in a given mode.

Dispatches on path extension. Directory paths open a `FilesDaf`; paths
ending in `.h5df` or containing `.h5dfs#<group>` are reserved for an
H5df backend (not implemented this slice).

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
#>  @ internal              :<environment: 0x56540f36ec70> 
#>  @ cache                 :<environment: 0x56540f36cb58> 
#>  @ axis_version_counter  :<environment: 0x56540f366ea0> 
#>  @ vector_version_counter:<environment: 0x56540f3637e0> 
#>  @ matrix_version_counter:<environment: 0x56540f3642d0> 
d <- open_daf(tmp, "r")
```
