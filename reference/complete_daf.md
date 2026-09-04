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
#>  @ internal              :<environment: 0x55bbda7289e8> 
#>  @ cache                 :<environment: 0x55bbda7258a0> 
#>  @ axis_version_counter  :<environment: 0x55bbda723590> 
#>  @ vector_version_counter:<environment: 0x55bbda723868> 
#>  @ matrix_version_counter:<environment: 0x55bbda723b40> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55bbde88e950> 
#>  @ cache                 :<environment: 0x55bbde88f130> 
#>  @ axis_version_counter  :<environment: 0x55bbde889d70> 
#>  @ vector_version_counter:<environment: 0x55bbde884790> 
#>  @ matrix_version_counter:<environment: 0x55bbde885280> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55bbdebf83c0> 
#>  ..  ..@ cache                 :<environment: 0x55bbdebcee50> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55bbdebcac90> 
#>  ..  ..@ vector_version_counter:<environment: 0x55bbdebcaf68> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55bbdebcb240> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55bbdec82128> 
#>  ..  ..@ cache                 :<environment: 0x55bbdec82e10> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55bbdec7ebe0> 
#>  ..  ..@ vector_version_counter:<environment: 0x55bbdec7eeb8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55bbdec7f190> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55bbdec82128> 
#>  .. @ cache                 :<environment: 0x55bbdec82e10> 
#>  .. @ axis_version_counter  :<environment: 0x55bbdec7ebe0> 
#>  .. @ vector_version_counter:<environment: 0x55bbdec7eeb8> 
#>  .. @ matrix_version_counter:<environment: 0x55bbdec7f190> 
chain <- complete_daf(new_dir, "r")
```
