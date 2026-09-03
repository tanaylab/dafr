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
#>  @ internal              :<environment: 0x562453871718> 
#>  @ cache                 :<environment: 0x562453872400> 
#>  @ axis_version_counter  :<environment: 0x5624538700f0> 
#>  @ vector_version_counter:<environment: 0x5624538703c8> 
#>  @ matrix_version_counter:<environment: 0x5624538706a0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x562456bbad58> 
#>  @ cache                 :<environment: 0x562456bbb0a0> 
#>  @ axis_version_counter  :<environment: 0x562456baaf88> 
#>  @ vector_version_counter:<environment: 0x562456bab260> 
#>  @ matrix_version_counter:<environment: 0x562456babe30> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x562456dbeb38> 
#>  ..  ..@ cache                 :<environment: 0x562456da15b8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562456da30d8> 
#>  ..  ..@ vector_version_counter:<environment: 0x562456d9f580> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562456d9f858> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x562456e3e4f0> 
#>  ..  ..@ cache                 :<environment: 0x562456e3b3a8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562456e39098> 
#>  ..  ..@ vector_version_counter:<environment: 0x562456e39370> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562456e39648> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x562456e3e4f0> 
#>  .. @ cache                 :<environment: 0x562456e3b3a8> 
#>  .. @ axis_version_counter  :<environment: 0x562456e39098> 
#>  .. @ vector_version_counter:<environment: 0x562456e39370> 
#>  .. @ matrix_version_counter:<environment: 0x562456e39648> 
chain <- complete_daf(new_dir, "r")
```
