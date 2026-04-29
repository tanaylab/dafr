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
#>  @ internal              :<environment: 0x55e7107b5410> 
#>  @ cache                 :<environment: 0x55e7107b5f38> 
#>  @ axis_version_counter  :<environment: 0x55e7107b3c28> 
#>  @ vector_version_counter:<environment: 0x55e7107b3f00> 
#>  @ matrix_version_counter:<environment: 0x55e7107b41d8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55e7105fd938> 
#>  @ cache                 :<environment: 0x55e7105fdc80> 
#>  @ axis_version_counter  :<environment: 0x55e7105fb970> 
#>  @ vector_version_counter:<environment: 0x55e7105fbc48> 
#>  @ matrix_version_counter:<environment: 0x55e7105fbf20> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55e710674758> 
#>  ..  ..@ cache                 :<environment: 0x55e71065ccd0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55e71065a9c0> 
#>  ..  ..@ vector_version_counter:<environment: 0x55e71065ac98> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55e71065af70> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55e7106dcfc8> 
#>  ..  ..@ cache                 :<environment: 0x55e7106ddaf0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55e7106db7e0> 
#>  ..  ..@ vector_version_counter:<environment: 0x55e7106dbab8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55e7106dbd90> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55e7106dcfc8> 
#>  .. @ cache                 :<environment: 0x55e7106ddaf0> 
#>  .. @ axis_version_counter  :<environment: 0x55e7106db7e0> 
#>  .. @ vector_version_counter:<environment: 0x55e7106dbab8> 
#>  .. @ matrix_version_counter:<environment: 0x55e7106dbd90> 
chain <- complete_daf(new_dir, "r")
```
