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
#>  @ internal              :<environment: 0x562ac8a022e8> 
#>  @ cache                 :<environment: 0x562ac8a017c0> 
#>  @ axis_version_counter  :<environment: 0x562ac8a03ad0> 
#>  @ vector_version_counter:<environment: 0x562ac8a037f8> 
#>  @ matrix_version_counter:<environment: 0x562ac8a03520> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x562accafb548> 
#>  @ cache                 :<environment: 0x562accafb200> 
#>  @ axis_version_counter  :<environment: 0x562accaff430> 
#>  @ vector_version_counter:<environment: 0x562accaff158> 
#>  @ matrix_version_counter:<environment: 0x562accafee80> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x562acbc013a0> 
#>  ..  ..@ cache                 :<environment: 0x562accb1a668> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562accb1c978> 
#>  ..  ..@ vector_version_counter:<environment: 0x562accb1c6a0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562accb1c3c8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x562ac9e19ce8> 
#>  ..  ..@ cache                 :<environment: 0x562ac9e191c0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562ac9e1b4d0> 
#>  ..  ..@ vector_version_counter:<environment: 0x562ac9e1b1f8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562ac9e1ed50> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x562ac9e19ce8> 
#>  .. @ cache                 :<environment: 0x562ac9e191c0> 
#>  .. @ axis_version_counter  :<environment: 0x562ac9e1b4d0> 
#>  .. @ vector_version_counter:<environment: 0x562ac9e1b1f8> 
#>  .. @ matrix_version_counter:<environment: 0x562ac9e1ed50> 
chain <- complete_daf(new_dir, "r")
```
