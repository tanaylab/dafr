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
#>  @ internal              :<environment: 0x5584a7a5b1b8> 
#>  @ cache                 :<environment: 0x5584a7a56150> 
#>  @ axis_version_counter  :<environment: 0x5584a7a57c70> 
#>  @ vector_version_counter:<environment: 0x5584a7a54118> 
#>  @ matrix_version_counter:<environment: 0x5584a7a543f0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x5584a9563280> 
#>  @ cache                 :<environment: 0x5584a955f5c8> 
#>  @ axis_version_counter  :<environment: 0x5584a95562b0> 
#>  @ vector_version_counter:<environment: 0x5584a9551130> 
#>  @ matrix_version_counter:<environment: 0x5584a9551408> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x5584a9846920> 
#>  ..  ..@ cache                 :<environment: 0x5584a9829250> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5584a9826f40> 
#>  ..  ..@ vector_version_counter:<environment: 0x5584a9827218> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5584a98274f0> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x5584a98d0480> 
#>  ..  ..@ cache                 :<environment: 0x5584a98cd338> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5584a98cb098> 
#>  ..  ..@ vector_version_counter:<environment: 0x5584a98cb370> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5584a98cb648> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x5584a98d0480> 
#>  .. @ cache                 :<environment: 0x5584a98cd338> 
#>  .. @ axis_version_counter  :<environment: 0x5584a98cb098> 
#>  .. @ vector_version_counter:<environment: 0x5584a98cb370> 
#>  .. @ matrix_version_counter:<environment: 0x5584a98cb648> 
chain <- complete_daf(new_dir, "r")
```
