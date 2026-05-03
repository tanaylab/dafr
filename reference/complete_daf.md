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
#>  @ internal              :<environment: 0x557ebaf89518> 
#>  @ cache                 :<environment: 0x557ebaf889f0> 
#>  @ axis_version_counter  :<environment: 0x557ebaf8ad00> 
#>  @ vector_version_counter:<environment: 0x557ebaf8aa28> 
#>  @ matrix_version_counter:<environment: 0x557ebaf8a750> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x557eba49ed60> 
#>  @ cache                 :<environment: 0x557eba49ea18> 
#>  @ axis_version_counter  :<environment: 0x557eba4aebb8> 
#>  @ vector_version_counter:<environment: 0x557eba4ae8e0> 
#>  @ matrix_version_counter:<environment: 0x557eba4ae608> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x557ebfbc7c38> 
#>  ..  ..@ cache                 :<environment: 0x557ebfc716e8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x557ebfc6fbc8> 
#>  ..  ..@ vector_version_counter:<environment: 0x557ebf9e3290> 
#>  ..  ..@ matrix_version_counter:<environment: 0x557ebf9e2fb8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x557ebc6d7e68> 
#>  ..  ..@ cache                 :<environment: 0x557ebc6d7340> 
#>  ..  ..@ axis_version_counter  :<environment: 0x557ebc6d9650> 
#>  ..  ..@ vector_version_counter:<environment: 0x557ebc6d9378> 
#>  ..  ..@ matrix_version_counter:<environment: 0x557ebc6d90a0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x557ebc6d7e68> 
#>  .. @ cache                 :<environment: 0x557ebc6d7340> 
#>  .. @ axis_version_counter  :<environment: 0x557ebc6d9650> 
#>  .. @ vector_version_counter:<environment: 0x557ebc6d9378> 
#>  .. @ matrix_version_counter:<environment: 0x557ebc6d90a0> 
chain <- complete_daf(new_dir, "r")
```
