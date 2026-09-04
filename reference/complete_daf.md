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
#>  @ internal              :<environment: 0x555ae7e2a910> 
#>  @ cache                 :<environment: 0x555ae7e277c8> 
#>  @ axis_version_counter  :<environment: 0x555ae7e254b8> 
#>  @ vector_version_counter:<environment: 0x555ae7e25790> 
#>  @ matrix_version_counter:<environment: 0x555ae7e25a68> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x555af054f160> 
#>  @ cache                 :<environment: 0x555af054fd30> 
#>  @ axis_version_counter  :<environment: 0x555af0546ec0> 
#>  @ vector_version_counter:<environment: 0x555af0547a90> 
#>  @ matrix_version_counter:<environment: 0x555af05485b8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x555af08b7110> 
#>  ..  ..@ cache                 :<environment: 0x555af088dba0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x555af0889970> 
#>  ..  ..@ vector_version_counter:<environment: 0x555af0889c48> 
#>  ..  ..@ matrix_version_counter:<environment: 0x555af0889f20> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x555af0940e40> 
#>  ..  ..@ cache                 :<environment: 0x555af0941b28> 
#>  ..  ..@ axis_version_counter  :<environment: 0x555af093f818> 
#>  ..  ..@ vector_version_counter:<environment: 0x555af093faf0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x555af093fdc8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x555af0940e40> 
#>  .. @ cache                 :<environment: 0x555af0941b28> 
#>  .. @ axis_version_counter  :<environment: 0x555af093f818> 
#>  .. @ vector_version_counter:<environment: 0x555af093faf0> 
#>  .. @ matrix_version_counter:<environment: 0x555af093fdc8> 
chain <- complete_daf(new_dir, "r")
```
