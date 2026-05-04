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
#>  @ internal              :<environment: 0x55c10c9bf3f8> 
#>  @ cache                 :<environment: 0x55c10c9be8d0> 
#>  @ axis_version_counter  :<environment: 0x55c10c9c0be0> 
#>  @ vector_version_counter:<environment: 0x55c10c9c0908> 
#>  @ matrix_version_counter:<environment: 0x55c10c9c0630> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55c10e15b580> 
#>  @ cache                 :<environment: 0x55c10e15b238> 
#>  @ axis_version_counter  :<environment: 0x55c10e0fcb08> 
#>  @ vector_version_counter:<environment: 0x55c10e0fc830> 
#>  @ matrix_version_counter:<environment: 0x55c10e0fc558> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55c10f42da00> 
#>  ..  ..@ cache                 :<environment: 0x55c107beb720> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55c100d4ca70> 
#>  ..  ..@ vector_version_counter:<environment: 0x55c100d4c798> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55c100d4c4c0> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55c10c4e1440> 
#>  ..  ..@ cache                 :<environment: 0x55c10c4e0918> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55c10c4e2c28> 
#>  ..  ..@ vector_version_counter:<environment: 0x55c10c4e2950> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55c10c476658> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55c10c4e1440> 
#>  .. @ cache                 :<environment: 0x55c10c4e0918> 
#>  .. @ axis_version_counter  :<environment: 0x55c10c4e2c28> 
#>  .. @ vector_version_counter:<environment: 0x55c10c4e2950> 
#>  .. @ matrix_version_counter:<environment: 0x55c10c476658> 
chain <- complete_daf(new_dir, "r")
```
