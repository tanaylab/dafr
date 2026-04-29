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
#>  @ internal              :<environment: 0x55f3d3691310> 
#>  @ cache                 :<environment: 0x55f3d368e008> 
#>  @ axis_version_counter  :<environment: 0x55f3d368bcf8> 
#>  @ vector_version_counter:<environment: 0x55f3d368bfd0> 
#>  @ matrix_version_counter:<environment: 0x55f3d368c2a8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55f3d34c69c8> 
#>  @ cache                 :<environment: 0x55f3d34c6d10> 
#>  @ axis_version_counter  :<environment: 0x55f3d34c2ae0> 
#>  @ vector_version_counter:<environment: 0x55f3d34c2db8> 
#>  @ matrix_version_counter:<environment: 0x55f3d34c3090> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55f3d3537a18> 
#>  ..  ..@ cache                 :<environment: 0x55f3d351ff90> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55f3d3521ab0> 
#>  ..  ..@ vector_version_counter:<environment: 0x55f3d351df58> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55f3d351e230> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55f3d35abe40> 
#>  ..  ..@ cache                 :<environment: 0x55f3d35a6bb8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55f3d35a48e0> 
#>  ..  ..@ vector_version_counter:<environment: 0x55f3d35a4bb8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55f3d35a4e90> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55f3d35abe40> 
#>  .. @ cache                 :<environment: 0x55f3d35a6bb8> 
#>  .. @ axis_version_counter  :<environment: 0x55f3d35a48e0> 
#>  .. @ vector_version_counter:<environment: 0x55f3d35a4bb8> 
#>  .. @ matrix_version_counter:<environment: 0x55f3d35a4e90> 
chain <- complete_daf(new_dir, "r")
```
