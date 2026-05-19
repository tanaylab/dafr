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
#>  @ internal              :<environment: 0x55fccef59ff8> 
#>  @ cache                 :<environment: 0x55fccef5d300> 
#>  @ axis_version_counter  :<environment: 0x55fccef5f610> 
#>  @ vector_version_counter:<environment: 0x55fccef5f338> 
#>  @ matrix_version_counter:<environment: 0x55fccef5f060> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55fcc74680d8> 
#>  @ cache                 :<environment: 0x55fcc7467d90> 
#>  @ axis_version_counter  :<environment: 0x55fcc746a0a0> 
#>  @ vector_version_counter:<environment: 0x55fcc7469dc8> 
#>  @ matrix_version_counter:<environment: 0x55fcc7469af0> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55fcc7d8cbe0> 
#>  ..  ..@ cache                 :<environment: 0x55fcceb97090> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55fcceb993a0> 
#>  ..  ..@ vector_version_counter:<environment: 0x55fcceb990c8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55fcceb98df0> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55fccd9b6c18> 
#>  ..  ..@ cache                 :<environment: 0x55fccd9b9f20> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55fccd9bc230> 
#>  ..  ..@ vector_version_counter:<environment: 0x55fccd9bbf58> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55fccd9bbc80> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55fccd9b6c18> 
#>  .. @ cache                 :<environment: 0x55fccd9b9f20> 
#>  .. @ axis_version_counter  :<environment: 0x55fccd9bc230> 
#>  .. @ vector_version_counter:<environment: 0x55fccd9bbf58> 
#>  .. @ matrix_version_counter:<environment: 0x55fccd9bbc80> 
chain <- complete_daf(new_dir, "r")
```
