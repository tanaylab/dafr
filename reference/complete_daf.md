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
#>  @ internal              :<environment: 0x5556075f7dc0> 
#>  @ cache                 :<environment: 0x5556075faf08> 
#>  @ axis_version_counter  :<environment: 0x555607625fb8> 
#>  @ vector_version_counter:<environment: 0x555607625ce0> 
#>  @ matrix_version_counter:<environment: 0x555607625a08> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55560d039508> 
#>  @ cache                 :<environment: 0x55560d0391c0> 
#>  @ axis_version_counter  :<environment: 0x55560723cd20> 
#>  @ vector_version_counter:<environment: 0x55560723ca48> 
#>  @ matrix_version_counter:<environment: 0x5556072405a0> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55560af20b38> 
#>  ..  ..@ cache                 :<environment: 0x55560c7189c8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55560c71abf8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55560c71a920> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55560c71a648> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55560bfcdc28> 
#>  ..  ..@ cache                 :<environment: 0x55560bfccf40> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55560bfcf250> 
#>  ..  ..@ vector_version_counter:<environment: 0x55560bfcef78> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55560bfd2ad0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55560bfcdc28> 
#>  .. @ cache                 :<environment: 0x55560bfccf40> 
#>  .. @ axis_version_counter  :<environment: 0x55560bfcf250> 
#>  .. @ vector_version_counter:<environment: 0x55560bfcef78> 
#>  .. @ matrix_version_counter:<environment: 0x55560bfd2ad0> 
chain <- complete_daf(new_dir, "r")
```
