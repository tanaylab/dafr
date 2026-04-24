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
#>  @ internal              :<environment: 0x55fd48ebb780> 
#>  @ cache                 :<environment: 0x55fd48eb8478> 
#>  @ axis_version_counter  :<environment: 0x55fd48eb62f0> 
#>  @ vector_version_counter:<environment: 0x55fd48eb68d8> 
#>  @ matrix_version_counter:<environment: 0x55fd48eb6d00> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55fd4700b8a0> 
#>  @ cache                 :<environment: 0x55fd4700bbe8> 
#>  @ axis_version_counter  :<environment: 0x55fd470098d8> 
#>  @ vector_version_counter:<environment: 0x55fd47009bb0> 
#>  @ matrix_version_counter:<environment: 0x55fd47009e88> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55fd4314f4a8> 
#>  ..  ..@ cache                 :<environment: 0x55fd4455fe68> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55fd4455db58> 
#>  ..  ..@ vector_version_counter:<environment: 0x55fd4455de30> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55fd4455e108> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55fd485dce98> 
#>  ..  ..@ cache                 :<environment: 0x55fd485dd9c0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55fd485db6b0> 
#>  ..  ..@ vector_version_counter:<environment: 0x55fd485db988> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55fd485dbc60> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55fd485dce98> 
#>  .. @ cache                 :<environment: 0x55fd485dd9c0> 
#>  .. @ axis_version_counter  :<environment: 0x55fd485db6b0> 
#>  .. @ vector_version_counter:<environment: 0x55fd485db988> 
#>  .. @ matrix_version_counter:<environment: 0x55fd485dbc60> 
chain <- complete_daf(new_dir, "r")
```
