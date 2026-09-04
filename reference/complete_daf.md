# Reopen a persistent chain from disk.

Opens the complete chain of repositories rooted at `leaf` by following
the `base_daf_repository` scalar of each. Returns a
[`chain_reader()`](https://tanaylab.github.io/dafr/reference/chain_reader.md)
(`mode = "r"`) or
[`chain_writer()`](https://tanaylab.github.io/dafr/reference/chain_writer.md)
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

## Details

A repository records only its own immediate bases; what each of them
rests on is recorded in it. A repository reached through more than one
of them appears once, at its earliest position, so that a base never
overrides what rests on it.

## Examples

``` r
tmp_root <- tempfile(); dir.create(tmp_root)
base_dir <- file.path(tmp_root, "base")
new_dir <- file.path(tmp_root, "new")
files_daf(base_dir, name = "base", mode = "w+")
#> <dafr::FilesDaf>
#>  @ name                  : chr "base"
#>  @ internal              :<environment: 0x5602c10b0de0> 
#>  @ cache                 :<environment: 0x5602c10b1ac8> 
#>  @ axis_version_counter  :<environment: 0x5602c10af7b8> 
#>  @ vector_version_counter:<environment: 0x5602c10afa90> 
#>  @ matrix_version_counter:<environment: 0x5602c10afd68> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x5602c9bc91c0> 
#>  @ cache                 :<environment: 0x5602c9bc9e38> 
#>  @ axis_version_counter  :<environment: 0x5602c9bc0fc8> 
#>  @ vector_version_counter:<environment: 0x5602c9bc1af0> 
#>  @ matrix_version_counter:<environment: 0x5602c9bc1dc8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x5602c9f30920> 
#>  ..  ..@ cache                 :<environment: 0x5602c9f073b0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5602c9f03180> 
#>  ..  ..@ vector_version_counter:<environment: 0x5602c9f03458> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5602c9f03730> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x5602c9fba650> 
#>  ..  ..@ cache                 :<environment: 0x5602c9fbb338> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5602c9fb9028> 
#>  ..  ..@ vector_version_counter:<environment: 0x5602c9fb9300> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5602c9fb95d8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x5602c9fba650> 
#>  .. @ cache                 :<environment: 0x5602c9fbb338> 
#>  .. @ axis_version_counter  :<environment: 0x5602c9fb9028> 
#>  .. @ vector_version_counter:<environment: 0x5602c9fb9300> 
#>  .. @ matrix_version_counter:<environment: 0x5602c9fb95d8> 
chain <- complete_daf(new_dir, "r")
```
