# Reopen a persistent chain from disk.

Walks the `base_daf_repository` scalar chain rooted at `leaf`, opening
each level with
[`open_daf()`](https://tanaylab.github.io/dafr/reference/open_daf.md).
Returns a `chain_reader` (`mode = "r"`) or `chain_writer`
(`mode = "r+"`, only the leaf is writable).

## Usage

``` r
complete_daf(leaf, mode = "r", name = NULL)
```

## Arguments

- leaf:

  Filesystem path to the leaf daf.

- mode:

  `"r"` or `"r+"`.

- name:

  Optional name.

## Value

A `DafReader` or `DafWriter`.

## Examples

``` r
tmp_root <- tempfile(); dir.create(tmp_root)
base_dir <- file.path(tmp_root, "base")
new_dir <- file.path(tmp_root, "new")
files_daf(base_dir, name = "base", mode = "w+")
#> <dafr::FilesDaf>
#>  @ name                  : chr "base"
#>  @ internal              :<environment: 0x55b7bb5cf4e0> 
#>  @ cache                 :<environment: 0x55b7bb5cc1d8> 
#>  @ axis_version_counter  :<environment: 0x55b7bb5c9ec8> 
#>  @ vector_version_counter:<environment: 0x55b7bb5ca1a0> 
#>  @ matrix_version_counter:<environment: 0x55b7bb5ca478> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55b7b6c812e0> 
#>  @ cache                 :<environment: 0x55b7b6c81628> 
#>  @ axis_version_counter  :<environment: 0x55b7b6c7d3f8> 
#>  @ vector_version_counter:<environment: 0x55b7b6c7d6d0> 
#>  @ matrix_version_counter:<environment: 0x55b7b6c7d9a8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55b7b6cfa190> 
#>  ..  ..@ cache                 :<environment: 0x55b7b6ce2660> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55b7b6ce0350> 
#>  ..  ..@ vector_version_counter:<environment: 0x55b7b6ce0628> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55b7b6ce0900> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55b7bb4e95c0> 
#>  ..  ..@ cache                 :<environment: 0x55b7bb4ea0e8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55b7bb4e7dd8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55b7bb4e80b0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55b7bb4e8388> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55b7bb4e95c0> 
#>  .. @ cache                 :<environment: 0x55b7bb4ea0e8> 
#>  .. @ axis_version_counter  :<environment: 0x55b7bb4e7dd8> 
#>  .. @ vector_version_counter:<environment: 0x55b7bb4e80b0> 
#>  .. @ matrix_version_counter:<environment: 0x55b7bb4e8388> 
chain <- complete_daf(new_dir, "r")
```
