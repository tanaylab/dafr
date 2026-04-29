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
#>  @ internal              :<environment: 0x562e29f19408> 
#>  @ cache                 :<environment: 0x562e29f19f30> 
#>  @ axis_version_counter  :<environment: 0x562e29f15d00> 
#>  @ vector_version_counter:<environment: 0x562e29f15fd8> 
#>  @ matrix_version_counter:<environment: 0x562e29f162b0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x562e29d61930> 
#>  @ cache                 :<environment: 0x562e29d61c78> 
#>  @ axis_version_counter  :<environment: 0x562e29d5da48> 
#>  @ vector_version_counter:<environment: 0x562e29d5dd20> 
#>  @ matrix_version_counter:<environment: 0x562e29d5dff8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x562e29dd6830> 
#>  ..  ..@ cache                 :<environment: 0x562e29dbeda8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562e29dbca98> 
#>  ..  ..@ vector_version_counter:<environment: 0x562e29dbcd70> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562e29dbd048> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x562e29e3f0a0> 
#>  ..  ..@ cache                 :<environment: 0x562e29e3fbc8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562e29e3d8b8> 
#>  ..  ..@ vector_version_counter:<environment: 0x562e29e3db90> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562e29e3de68> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x562e29e3f0a0> 
#>  .. @ cache                 :<environment: 0x562e29e3fbc8> 
#>  .. @ axis_version_counter  :<environment: 0x562e29e3d8b8> 
#>  .. @ vector_version_counter:<environment: 0x562e29e3db90> 
#>  .. @ matrix_version_counter:<environment: 0x562e29e3de68> 
chain <- complete_daf(new_dir, "r")
```
