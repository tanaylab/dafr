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
#>  @ internal              :<environment: 0x56427585fc38> 
#>  @ cache                 :<environment: 0x56427585f110> 
#>  @ axis_version_counter  :<environment: 0x564275861420> 
#>  @ vector_version_counter:<environment: 0x564275861148> 
#>  @ matrix_version_counter:<environment: 0x564275860e70> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x5642761611c0> 
#>  @ cache                 :<environment: 0x564276160e78> 
#>  @ axis_version_counter  :<environment: 0x56427a09f928> 
#>  @ vector_version_counter:<environment: 0x56427a09f650> 
#>  @ matrix_version_counter:<environment: 0x56427a09f378> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x56427cf8a9d8> 
#>  ..  ..@ cache                 :<environment: 0x56427cfce318> 
#>  ..  ..@ axis_version_counter  :<environment: 0x564272fc2098> 
#>  ..  ..@ vector_version_counter:<environment: 0x564272fc1dc0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x564272fc1ae8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x56427ca1acb8> 
#>  ..  ..@ cache                 :<environment: 0x56427ca1a190> 
#>  ..  ..@ axis_version_counter  :<environment: 0x56427ca1c4a0> 
#>  ..  ..@ vector_version_counter:<environment: 0x56427ca1c1c8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x56427ca1bef0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x56427ca1acb8> 
#>  .. @ cache                 :<environment: 0x56427ca1a190> 
#>  .. @ axis_version_counter  :<environment: 0x56427ca1c4a0> 
#>  .. @ vector_version_counter:<environment: 0x56427ca1c1c8> 
#>  .. @ matrix_version_counter:<environment: 0x56427ca1bef0> 
chain <- complete_daf(new_dir, "r")
```
