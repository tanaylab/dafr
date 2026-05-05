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
#>  @ internal              :<environment: 0x558aaf29d7a0> 
#>  @ cache                 :<environment: 0x558aaf29cc78> 
#>  @ axis_version_counter  :<environment: 0x558aaf0fafa8> 
#>  @ vector_version_counter:<environment: 0x558aaf0facd0> 
#>  @ matrix_version_counter:<environment: 0x558aaf0fa9f8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x558ab066b1e8> 
#>  @ cache                 :<environment: 0x558ab066aea0> 
#>  @ axis_version_counter  :<environment: 0x558ab066d1b0> 
#>  @ vector_version_counter:<environment: 0x558ab066ced8> 
#>  @ matrix_version_counter:<environment: 0x558ab0645d70> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x558ab3f396d0> 
#>  ..  ..@ cache                 :<environment: 0x558ab04cb660> 
#>  ..  ..@ axis_version_counter  :<environment: 0x558ab558ed60> 
#>  ..  ..@ vector_version_counter:<environment: 0x558ab558ea88> 
#>  ..  ..@ matrix_version_counter:<environment: 0x558ab558e7b0> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x558ab54602f0> 
#>  ..  ..@ cache                 :<environment: 0x558ab545f7c8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x558ab21eae48> 
#>  ..  ..@ vector_version_counter:<environment: 0x558ab21eab70> 
#>  ..  ..@ matrix_version_counter:<environment: 0x558ab21ea898> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x558ab54602f0> 
#>  .. @ cache                 :<environment: 0x558ab545f7c8> 
#>  .. @ axis_version_counter  :<environment: 0x558ab21eae48> 
#>  .. @ vector_version_counter:<environment: 0x558ab21eab70> 
#>  .. @ matrix_version_counter:<environment: 0x558ab21ea898> 
chain <- complete_daf(new_dir, "r")
```
