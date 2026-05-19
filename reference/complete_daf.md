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
#>  @ internal              :<environment: 0x55a18d87a9f0> 
#>  @ cache                 :<environment: 0x55a18d87dcf8> 
#>  @ axis_version_counter  :<environment: 0x55a18d881f28> 
#>  @ vector_version_counter:<environment: 0x55a18d881c50> 
#>  @ matrix_version_counter:<environment: 0x55a18d881978> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55a1942e85a8> 
#>  @ cache                 :<environment: 0x55a1942e8260> 
#>  @ axis_version_counter  :<environment: 0x55a1942ea570> 
#>  @ vector_version_counter:<environment: 0x55a1942ea298> 
#>  @ matrix_version_counter:<environment: 0x55a1942e9fc0> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55a1934dd618> 
#>  ..  ..@ cache                 :<environment: 0x55a1930fa500> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55a1930fc810> 
#>  ..  ..@ vector_version_counter:<environment: 0x55a1930fc538> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55a1930fc260> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55a18da35f00> 
#>  ..  ..@ cache                 :<environment: 0x55a18da39208> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55a18da3d438> 
#>  ..  ..@ vector_version_counter:<environment: 0x55a18da3d160> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55a18da3ce88> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55a18da35f00> 
#>  .. @ cache                 :<environment: 0x55a18da39208> 
#>  .. @ axis_version_counter  :<environment: 0x55a18da3d438> 
#>  .. @ vector_version_counter:<environment: 0x55a18da3d160> 
#>  .. @ matrix_version_counter:<environment: 0x55a18da3ce88> 
chain <- complete_daf(new_dir, "r")
```
