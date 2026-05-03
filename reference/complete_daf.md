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
#>  @ internal              :<environment: 0x55d6d570c688> 
#>  @ cache                 :<environment: 0x55d6d570bb60> 
#>  @ axis_version_counter  :<environment: 0x55d6d570de70> 
#>  @ vector_version_counter:<environment: 0x55d6d570db98> 
#>  @ matrix_version_counter:<environment: 0x55d6d570d8c0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55d6cf391030> 
#>  @ cache                 :<environment: 0x55d6cf390ce8> 
#>  @ axis_version_counter  :<environment: 0x55d6cf392ff8> 
#>  @ vector_version_counter:<environment: 0x55d6cf392d20> 
#>  @ matrix_version_counter:<environment: 0x55d6cf392a48> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55d6d92597c8> 
#>  ..  ..@ cache                 :<environment: 0x55d6d251e308> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55d6d2520618> 
#>  ..  ..@ vector_version_counter:<environment: 0x55d6d2520340> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55d6d2520068> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55d6d1c02b38> 
#>  ..  ..@ cache                 :<environment: 0x55d6d1c02010> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55d6d1c04320> 
#>  ..  ..@ vector_version_counter:<environment: 0x55d6d1c04048> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55d6d1c03d70> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55d6d1c02b38> 
#>  .. @ cache                 :<environment: 0x55d6d1c02010> 
#>  .. @ axis_version_counter  :<environment: 0x55d6d1c04320> 
#>  .. @ vector_version_counter:<environment: 0x55d6d1c04048> 
#>  .. @ matrix_version_counter:<environment: 0x55d6d1c03d70> 
chain <- complete_daf(new_dir, "r")
```
