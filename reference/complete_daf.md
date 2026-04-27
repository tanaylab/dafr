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
#>  @ internal              :<environment: 0x55c347ec27d8> 
#>  @ cache                 :<environment: 0x55c347ec0570> 
#>  @ axis_version_counter  :<environment: 0x55c347ebca08> 
#>  @ vector_version_counter:<environment: 0x55c347ebcce0> 
#>  @ matrix_version_counter:<environment: 0x55c347ebcfb8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55c347cfc190> 
#>  @ cache                 :<environment: 0x55c347cfc4d8> 
#>  @ axis_version_counter  :<environment: 0x55c347cfa1c8> 
#>  @ vector_version_counter:<environment: 0x55c347cfa4a0> 
#>  @ matrix_version_counter:<environment: 0x55c347cfa778> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55c347d6f170> 
#>  ..  ..@ cache                 :<environment: 0x55c347d57678> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55c347d55368> 
#>  ..  ..@ vector_version_counter:<environment: 0x55c347d55640> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55c347d55918> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55c347ddd7b0> 
#>  ..  ..@ cache                 :<environment: 0x55c347dde2d8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55c347ddbfc8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55c347ddc2a0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55c347ddc578> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55c347ddd7b0> 
#>  .. @ cache                 :<environment: 0x55c347dde2d8> 
#>  .. @ axis_version_counter  :<environment: 0x55c347ddbfc8> 
#>  .. @ vector_version_counter:<environment: 0x55c347ddc2a0> 
#>  .. @ matrix_version_counter:<environment: 0x55c347ddc578> 
chain <- complete_daf(new_dir, "r")
```
