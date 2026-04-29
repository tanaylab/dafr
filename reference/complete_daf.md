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
#>  @ internal              :<environment: 0x557e878396d0> 
#>  @ cache                 :<environment: 0x557e8783a1f8> 
#>  @ axis_version_counter  :<environment: 0x557e87837ee8> 
#>  @ vector_version_counter:<environment: 0x557e878381c0> 
#>  @ matrix_version_counter:<environment: 0x557e87838498> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x557e87681c68> 
#>  @ cache                 :<environment: 0x557e87681fb0> 
#>  @ axis_version_counter  :<environment: 0x557e8767fca0> 
#>  @ vector_version_counter:<environment: 0x557e8767ff78> 
#>  @ matrix_version_counter:<environment: 0x557e87680250> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x557e876f6b68> 
#>  ..  ..@ cache                 :<environment: 0x557e876df038> 
#>  ..  ..@ axis_version_counter  :<environment: 0x557e876e0b58> 
#>  ..  ..@ vector_version_counter:<environment: 0x557e876e0e30> 
#>  ..  ..@ matrix_version_counter:<environment: 0x557e876db3b8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x557e87767048> 
#>  ..  ..@ cache                 :<environment: 0x557e87763d40> 
#>  ..  ..@ axis_version_counter  :<environment: 0x557e87761a30> 
#>  ..  ..@ vector_version_counter:<environment: 0x557e87761d08> 
#>  ..  ..@ matrix_version_counter:<environment: 0x557e87761fe0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x557e87767048> 
#>  .. @ cache                 :<environment: 0x557e87763d40> 
#>  .. @ axis_version_counter  :<environment: 0x557e87761a30> 
#>  .. @ vector_version_counter:<environment: 0x557e87761d08> 
#>  .. @ matrix_version_counter:<environment: 0x557e87761fe0> 
chain <- complete_daf(new_dir, "r")
```
