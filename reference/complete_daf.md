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
#>  @ internal              :<environment: 0x5563349deec8> 
#>  @ cache                 :<environment: 0x5563349e21d0> 
#>  @ axis_version_counter  :<environment: 0x55632d590990> 
#>  @ vector_version_counter:<environment: 0x55632d5906b8> 
#>  @ matrix_version_counter:<environment: 0x55632d5903e0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x5563341bb640> 
#>  @ cache                 :<environment: 0x5563341bb2f8> 
#>  @ axis_version_counter  :<environment: 0x5563341bd608> 
#>  @ vector_version_counter:<environment: 0x5563341bd330> 
#>  @ matrix_version_counter:<environment: 0x5563341bd058> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x556330cb2dc0> 
#>  ..  ..@ cache                 :<environment: 0x55633004f320> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55632ffc6790> 
#>  ..  ..@ vector_version_counter:<environment: 0x55632ffc64b8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55632ffc61e0> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x5563375943a0> 
#>  ..  ..@ cache                 :<environment: 0x556337593878> 
#>  ..  ..@ axis_version_counter  :<environment: 0x556337604978> 
#>  ..  ..@ vector_version_counter:<environment: 0x5563376046a0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5563376043c8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x5563375943a0> 
#>  .. @ cache                 :<environment: 0x556337593878> 
#>  .. @ axis_version_counter  :<environment: 0x556337604978> 
#>  .. @ vector_version_counter:<environment: 0x5563376046a0> 
#>  .. @ matrix_version_counter:<environment: 0x5563376043c8> 
chain <- complete_daf(new_dir, "r")
```
