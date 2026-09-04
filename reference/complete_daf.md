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
#>  @ internal              :<environment: 0x5574ec426070> 
#>  @ cache                 :<environment: 0x5574ec422f28> 
#>  @ axis_version_counter  :<environment: 0x5574ec420c18> 
#>  @ vector_version_counter:<environment: 0x5574ec420ef0> 
#>  @ matrix_version_counter:<environment: 0x5574ec4211c8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x5574ee1b83c0> 
#>  @ cache                 :<environment: 0x5574ee1b8ba0> 
#>  @ axis_version_counter  :<environment: 0x5574ee1b37a8> 
#>  @ vector_version_counter:<environment: 0x5574ee1ae1c8> 
#>  @ matrix_version_counter:<environment: 0x5574ee1ae4a0> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x5574ee521df8> 
#>  ..  ..@ cache                 :<environment: 0x5574ee4f88c0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5574ee4f4690> 
#>  ..  ..@ vector_version_counter:<environment: 0x5574ee4f4968> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5574ee4f4c40> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x5574ee5abb60> 
#>  ..  ..@ cache                 :<environment: 0x5574ee5ac848> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5574ee5a8618> 
#>  ..  ..@ vector_version_counter:<environment: 0x5574ee5a88f0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5574ee5a8bc8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x5574ee5abb60> 
#>  .. @ cache                 :<environment: 0x5574ee5ac848> 
#>  .. @ axis_version_counter  :<environment: 0x5574ee5a8618> 
#>  .. @ vector_version_counter:<environment: 0x5574ee5a88f0> 
#>  .. @ matrix_version_counter:<environment: 0x5574ee5a8bc8> 
chain <- complete_daf(new_dir, "r")
```
