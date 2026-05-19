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
#>  @ internal              :<environment: 0x55d46e275620> 
#>  @ cache                 :<environment: 0x55d46e278928> 
#>  @ axis_version_counter  :<environment: 0x55d46e27ac38> 
#>  @ vector_version_counter:<environment: 0x55d46e27a960> 
#>  @ matrix_version_counter:<environment: 0x55d46e27a688> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55d46891c688> 
#>  @ cache                 :<environment: 0x55d46891c340> 
#>  @ axis_version_counter  :<environment: 0x55d46891e650> 
#>  @ vector_version_counter:<environment: 0x55d46891e378> 
#>  @ matrix_version_counter:<environment: 0x55d46891e0a0> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55d46f826b50> 
#>  ..  ..@ cache                 :<environment: 0x55d46eb63be0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55d46eb65ef0> 
#>  ..  ..@ vector_version_counter:<environment: 0x55d46eb65c18> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55d46eb65940> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55d468807810> 
#>  ..  ..@ cache                 :<environment: 0x55d468806ce8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55d468808ff8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55d468808d20> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55d468808a48> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55d468807810> 
#>  .. @ cache                 :<environment: 0x55d468806ce8> 
#>  .. @ axis_version_counter  :<environment: 0x55d468808ff8> 
#>  .. @ vector_version_counter:<environment: 0x55d468808d20> 
#>  .. @ matrix_version_counter:<environment: 0x55d468808a48> 
chain <- complete_daf(new_dir, "r")
```
