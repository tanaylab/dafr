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
#>  @ internal              :<environment: 0x55e70d617580> 
#>  @ cache                 :<environment: 0x55e70d6180a8> 
#>  @ axis_version_counter  :<environment: 0x55e70d613008> 
#>  @ vector_version_counter:<environment: 0x55e70d613af8> 
#>  @ matrix_version_counter:<environment: 0x55e70d614658> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55e70d44eb48> 
#>  @ cache                 :<environment: 0x55e70d44ee90> 
#>  @ axis_version_counter  :<environment: 0x55e70d44cb80> 
#>  @ vector_version_counter:<environment: 0x55e70d44ce58> 
#>  @ matrix_version_counter:<environment: 0x55e70d44d130> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55e70d4c1af0> 
#>  ..  ..@ cache                 :<environment: 0x55e70d4aa030> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55e70d4a7d20> 
#>  ..  ..@ vector_version_counter:<environment: 0x55e70d4a7ff8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55e70d4a82d0> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55e70d530130> 
#>  ..  ..@ cache                 :<environment: 0x55e70d530c90> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55e70d52e980> 
#>  ..  ..@ vector_version_counter:<environment: 0x55e70d52ec58> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55e70d52ef30> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55e70d530130> 
#>  .. @ cache                 :<environment: 0x55e70d530c90> 
#>  .. @ axis_version_counter  :<environment: 0x55e70d52e980> 
#>  .. @ vector_version_counter:<environment: 0x55e70d52ec58> 
#>  .. @ matrix_version_counter:<environment: 0x55e70d52ef30> 
chain <- complete_daf(new_dir, "r")
```
