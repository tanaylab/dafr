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
#>  @ internal              :<environment: 0x5643afc0bb98> 
#>  @ cache                 :<environment: 0x5643afc0eea0> 
#>  @ axis_version_counter  :<environment: 0x5643afc0d380> 
#>  @ vector_version_counter:<environment: 0x5643afc10ed8> 
#>  @ matrix_version_counter:<environment: 0x5643afc10c00> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x5643af1db530> 
#>  @ cache                 :<environment: 0x5643af1db1e8> 
#>  @ axis_version_counter  :<environment: 0x5643af1dd4f8> 
#>  @ vector_version_counter:<environment: 0x5643af1dd220> 
#>  @ matrix_version_counter:<environment: 0x5643af1dcf48> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x5643ae874be8> 
#>  ..  ..@ cache                 :<environment: 0x5643afeaf6f8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5643b041ab08> 
#>  ..  ..@ vector_version_counter:<environment: 0x5643b041a830> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5643b041a558> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x5643b2179ae8> 
#>  ..  ..@ cache                 :<environment: 0x5643b2110600> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5643b210eae0> 
#>  ..  ..@ vector_version_counter:<environment: 0x5643b210e808> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5643b2112360> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x5643b2179ae8> 
#>  .. @ cache                 :<environment: 0x5643b2110600> 
#>  .. @ axis_version_counter  :<environment: 0x5643b210eae0> 
#>  .. @ vector_version_counter:<environment: 0x5643b210e808> 
#>  .. @ matrix_version_counter:<environment: 0x5643b2112360> 
chain <- complete_daf(new_dir, "r")
```
