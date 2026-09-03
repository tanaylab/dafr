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
#>  @ internal              :<environment: 0x560b2332d308> 
#>  @ cache                 :<environment: 0x560b2332a1c0> 
#>  @ axis_version_counter  :<environment: 0x560b23327eb0> 
#>  @ vector_version_counter:<environment: 0x560b23328188> 
#>  @ matrix_version_counter:<environment: 0x560b23328460> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x560b2a3b70d0> 
#>  @ cache                 :<environment: 0x560b2a3b7840> 
#>  @ axis_version_counter  :<environment: 0x560b2a3b0598> 
#>  @ vector_version_counter:<environment: 0x560b2a3ace68> 
#>  @ matrix_version_counter:<environment: 0x560b2a3ad140> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x560b2a6c62a0> 
#>  ..  ..@ cache                 :<environment: 0x560b2a6a7e78> 
#>  ..  ..@ axis_version_counter  :<environment: 0x560b2a6a9998> 
#>  ..  ..@ vector_version_counter:<environment: 0x560b2a6a9c70> 
#>  ..  ..@ matrix_version_counter:<environment: 0x560b2a6a22d8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x560b2a74df98> 
#>  ..  ..@ cache                 :<environment: 0x560b2a74ec80> 
#>  ..  ..@ axis_version_counter  :<environment: 0x560b2a74c970> 
#>  ..  ..@ vector_version_counter:<environment: 0x560b2a74cc48> 
#>  ..  ..@ matrix_version_counter:<environment: 0x560b2a74cf20> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x560b2a74df98> 
#>  .. @ cache                 :<environment: 0x560b2a74ec80> 
#>  .. @ axis_version_counter  :<environment: 0x560b2a74c970> 
#>  .. @ vector_version_counter:<environment: 0x560b2a74cc48> 
#>  .. @ matrix_version_counter:<environment: 0x560b2a74cf20> 
chain <- complete_daf(new_dir, "r")
```
