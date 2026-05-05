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
#>  @ internal              :<environment: 0x564d7e64bd40> 
#>  @ cache                 :<environment: 0x564d7e64b218> 
#>  @ axis_version_counter  :<environment: 0x564d7e64d528> 
#>  @ vector_version_counter:<environment: 0x564d7e64d250> 
#>  @ matrix_version_counter:<environment: 0x564d7e64cf78> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x564d7d7c9d38> 
#>  @ cache                 :<environment: 0x564d7d7c99f0> 
#>  @ axis_version_counter  :<environment: 0x564d7d77ff20> 
#>  @ vector_version_counter:<environment: 0x564d7d77fc48> 
#>  @ matrix_version_counter:<environment: 0x564d7d77f970> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x564d811e6af0> 
#>  ..  ..@ cache                 :<environment: 0x564d8011f220> 
#>  ..  ..@ axis_version_counter  :<environment: 0x564d80121530> 
#>  ..  ..@ vector_version_counter:<environment: 0x564d80121258> 
#>  ..  ..@ matrix_version_counter:<environment: 0x564d80120f80> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x564d81fcd130> 
#>  ..  ..@ cache                 :<environment: 0x564d81fcc608> 
#>  ..  ..@ axis_version_counter  :<environment: 0x564d81fce918> 
#>  ..  ..@ vector_version_counter:<environment: 0x564d81fce640> 
#>  ..  ..@ matrix_version_counter:<environment: 0x564d81fce368> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x564d81fcd130> 
#>  .. @ cache                 :<environment: 0x564d81fcc608> 
#>  .. @ axis_version_counter  :<environment: 0x564d81fce918> 
#>  .. @ vector_version_counter:<environment: 0x564d81fce640> 
#>  .. @ matrix_version_counter:<environment: 0x564d81fce368> 
chain <- complete_daf(new_dir, "r")
```
