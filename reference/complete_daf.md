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
#>  @ internal              :<environment: 0x55fee06273e0> 
#>  @ cache                 :<environment: 0x55fee062a6e8> 
#>  @ axis_version_counter  :<environment: 0x55fee062c9f8> 
#>  @ vector_version_counter:<environment: 0x55fee062c720> 
#>  @ matrix_version_counter:<environment: 0x55fee062c448> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55fee245d3f0> 
#>  @ cache                 :<environment: 0x55fee245d0a8> 
#>  @ axis_version_counter  :<environment: 0x55fee245f3b8> 
#>  @ vector_version_counter:<environment: 0x55fee245f0e0> 
#>  @ matrix_version_counter:<environment: 0x55fee245ee08> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55fee3c0d840> 
#>  ..  ..@ cache                 :<environment: 0x55fee29e57e0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55fee29e7af0> 
#>  ..  ..@ vector_version_counter:<environment: 0x55fee29e7818> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55fee29e7540> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55fee20f23f0> 
#>  ..  ..@ cache                 :<environment: 0x55fee20f56f8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55fee20f7a08> 
#>  ..  ..@ vector_version_counter:<environment: 0x55fee20f7730> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55fee20f7458> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55fee20f23f0> 
#>  .. @ cache                 :<environment: 0x55fee20f56f8> 
#>  .. @ axis_version_counter  :<environment: 0x55fee20f7a08> 
#>  .. @ vector_version_counter:<environment: 0x55fee20f7730> 
#>  .. @ matrix_version_counter:<environment: 0x55fee20f7458> 
chain <- complete_daf(new_dir, "r")
```
