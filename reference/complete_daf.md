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
#>  @ internal              :<environment: 0x564259ae6970> 
#>  @ cache                 :<environment: 0x564259ae5e48> 
#>  @ axis_version_counter  :<environment: 0x564259ae8158> 
#>  @ vector_version_counter:<environment: 0x564259ae7e80> 
#>  @ matrix_version_counter:<environment: 0x564259ae7ba8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x56425d785680> 
#>  @ cache                 :<environment: 0x56425d785338> 
#>  @ axis_version_counter  :<environment: 0x56425d787648> 
#>  @ vector_version_counter:<environment: 0x56425d787370> 
#>  @ matrix_version_counter:<environment: 0x56425d787098> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x564260351290> 
#>  ..  ..@ cache                 :<environment: 0x56425ff4ea70> 
#>  ..  ..@ axis_version_counter  :<environment: 0x56425ff50d80> 
#>  ..  ..@ vector_version_counter:<environment: 0x56425ff50aa8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x56425ff507d0> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x564260da39f0> 
#>  ..  ..@ cache                 :<environment: 0x564260da6cf8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x564260da51d8> 
#>  ..  ..@ vector_version_counter:<environment: 0x564260da4f00> 
#>  ..  ..@ matrix_version_counter:<environment: 0x564260da8a58> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x564260da39f0> 
#>  .. @ cache                 :<environment: 0x564260da6cf8> 
#>  .. @ axis_version_counter  :<environment: 0x564260da51d8> 
#>  .. @ vector_version_counter:<environment: 0x564260da4f00> 
#>  .. @ matrix_version_counter:<environment: 0x564260da8a58> 
chain <- complete_daf(new_dir, "r")
```
