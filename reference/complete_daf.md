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
#>  @ internal              :<environment: 0x561d0fa353b8> 
#>  @ cache                 :<environment: 0x561d0fa34890> 
#>  @ axis_version_counter  :<environment: 0x561d0fa36ba0> 
#>  @ vector_version_counter:<environment: 0x561d0fa368c8> 
#>  @ matrix_version_counter:<environment: 0x561d0fa365f0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x561d0fa79fd8> 
#>  @ cache                 :<environment: 0x561d0fa79c90> 
#>  @ axis_version_counter  :<environment: 0x561d0fa7bfa0> 
#>  @ vector_version_counter:<environment: 0x561d0fa7bcc8> 
#>  @ matrix_version_counter:<environment: 0x561d0fa81740> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x561d0e6c92a8> 
#>  ..  ..@ cache                 :<environment: 0x561d0a803d38> 
#>  ..  ..@ axis_version_counter  :<environment: 0x561d0a806048> 
#>  ..  ..@ vector_version_counter:<environment: 0x561d0a805d70> 
#>  ..  ..@ matrix_version_counter:<environment: 0x561d0a805a98> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x561d11863c68> 
#>  ..  ..@ cache                 :<environment: 0x561d11863140> 
#>  ..  ..@ axis_version_counter  :<environment: 0x561d0b545e90> 
#>  ..  ..@ vector_version_counter:<environment: 0x561d0b545bb8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x561d0b5458e0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x561d11863c68> 
#>  .. @ cache                 :<environment: 0x561d11863140> 
#>  .. @ axis_version_counter  :<environment: 0x561d0b545e90> 
#>  .. @ vector_version_counter:<environment: 0x561d0b545bb8> 
#>  .. @ matrix_version_counter:<environment: 0x561d0b5458e0> 
chain <- complete_daf(new_dir, "r")
```
