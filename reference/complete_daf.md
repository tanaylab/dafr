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
#>  @ internal              :<environment: 0x55dcf6c0dff8> 
#>  @ cache                 :<environment: 0x55dcf6c0d4d0> 
#>  @ axis_version_counter  :<environment: 0x55dcf6c0f7e0> 
#>  @ vector_version_counter:<environment: 0x55dcf6c0f508> 
#>  @ matrix_version_counter:<environment: 0x55dcf6c0f230> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55dcfd8ae688> 
#>  @ cache                 :<environment: 0x55dcfd8b4090> 
#>  @ axis_version_counter  :<environment: 0x55dcfd8b2570> 
#>  @ vector_version_counter:<environment: 0x55dcfd8b60c8> 
#>  @ matrix_version_counter:<environment: 0x55dcfd8b5df0> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55dcf78b0fb8> 
#>  ..  ..@ cache                 :<environment: 0x55dcfcb792c8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55dcfcb7b5d8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55dcfcb7b300> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55dcfcb7ee58> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55dcfa6ef058> 
#>  ..  ..@ cache                 :<environment: 0x55dcfa6ee530> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55dcfa6f0840> 
#>  ..  ..@ vector_version_counter:<environment: 0x55dcfa6f0568> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55dcfa6f0290> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55dcfa6ef058> 
#>  .. @ cache                 :<environment: 0x55dcfa6ee530> 
#>  .. @ axis_version_counter  :<environment: 0x55dcfa6f0840> 
#>  .. @ vector_version_counter:<environment: 0x55dcfa6f0568> 
#>  .. @ matrix_version_counter:<environment: 0x55dcfa6f0290> 
chain <- complete_daf(new_dir, "r")
```
