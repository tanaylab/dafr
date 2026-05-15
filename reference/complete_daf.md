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
#>  @ internal              :<environment: 0x55f09c2b6b58> 
#>  @ cache                 :<environment: 0x55f09c2b9e60> 
#>  @ axis_version_counter  :<environment: 0x55f09c2bc170> 
#>  @ vector_version_counter:<environment: 0x55f09c2bbe98> 
#>  @ matrix_version_counter:<environment: 0x55f09c2bbbc0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55f09453de28> 
#>  @ cache                 :<environment: 0x55f09453dae0> 
#>  @ axis_version_counter  :<environment: 0x55f09453fdf0> 
#>  @ vector_version_counter:<environment: 0x55f09453fae0> 
#>  @ matrix_version_counter:<environment: 0x55f09b6a5ac8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55f09aa78a18> 
#>  ..  ..@ cache                 :<environment: 0x55f09bd08020> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55f09bd0a330> 
#>  ..  ..@ vector_version_counter:<environment: 0x55f09bd0a058> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55f09bd09d80> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55f09a62ef70> 
#>  ..  ..@ cache                 :<environment: 0x55f09a62e448> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55f09a630758> 
#>  ..  ..@ vector_version_counter:<environment: 0x55f09a630480> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55f09a6301a8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55f09a62ef70> 
#>  .. @ cache                 :<environment: 0x55f09a62e448> 
#>  .. @ axis_version_counter  :<environment: 0x55f09a630758> 
#>  .. @ vector_version_counter:<environment: 0x55f09a630480> 
#>  .. @ matrix_version_counter:<environment: 0x55f09a6301a8> 
chain <- complete_daf(new_dir, "r")
```
