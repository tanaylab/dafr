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
#>  @ internal              :<environment: 0x55fde80cffc8> 
#>  @ cache                 :<environment: 0x55fde80cf4a0> 
#>  @ axis_version_counter  :<environment: 0x55fde80d17b0> 
#>  @ vector_version_counter:<environment: 0x55fde80d14d8> 
#>  @ matrix_version_counter:<environment: 0x55fde80d1200> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55fde7a2b240> 
#>  @ cache                 :<environment: 0x55fde7a2ed28> 
#>  @ axis_version_counter  :<environment: 0x55fde7a2d208> 
#>  @ vector_version_counter:<environment: 0x55fde7a30d60> 
#>  @ matrix_version_counter:<environment: 0x55fde7a30a88> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55fde7b7d5c8> 
#>  ..  ..@ cache                 :<environment: 0x55fde7ba2b58> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55fde7ba4e68> 
#>  ..  ..@ vector_version_counter:<environment: 0x55fde7ba4b90> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55fde7fd7ce8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55fde6b5d6e8> 
#>  ..  ..@ cache                 :<environment: 0x55fde6b5cbc0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55fde6cc0f90> 
#>  ..  ..@ vector_version_counter:<environment: 0x55fde6cc0cb8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55fde6cc09e0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55fde6b5d6e8> 
#>  .. @ cache                 :<environment: 0x55fde6b5cbc0> 
#>  .. @ axis_version_counter  :<environment: 0x55fde6cc0f90> 
#>  .. @ vector_version_counter:<environment: 0x55fde6cc0cb8> 
#>  .. @ matrix_version_counter:<environment: 0x55fde6cc09e0> 
chain <- complete_daf(new_dir, "r")
```
