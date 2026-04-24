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
#>  @ internal              :<environment: 0x559d2c75b7d0> 
#>  @ cache                 :<environment: 0x559d2c7584c8> 
#>  @ axis_version_counter  :<environment: 0x559d2c756650> 
#>  @ vector_version_counter:<environment: 0x559d2c756ab0> 
#>  @ matrix_version_counter:<environment: 0x559d2c756d88> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x559d2e9fe430> 
#>  @ cache                 :<environment: 0x559d2e9fe778> 
#>  @ axis_version_counter  :<environment: 0x559d28353208> 
#>  @ vector_version_counter:<environment: 0x559d283534e0> 
#>  @ matrix_version_counter:<environment: 0x559d283537b8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x559d2ee929d8> 
#>  ..  ..@ cache                 :<environment: 0x559d2f5f9238> 
#>  ..  ..@ axis_version_counter  :<environment: 0x559d2f5f6f28> 
#>  ..  ..@ vector_version_counter:<environment: 0x559d2f5f7200> 
#>  ..  ..@ matrix_version_counter:<environment: 0x559d2f5f74d8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x559d2994e7e8> 
#>  ..  ..@ cache                 :<environment: 0x559d2994f310> 
#>  ..  ..@ axis_version_counter  :<environment: 0x559d2994d000> 
#>  ..  ..@ vector_version_counter:<environment: 0x559d2994d2d8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x559d2994d5b0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x559d2994e7e8> 
#>  .. @ cache                 :<environment: 0x559d2994f310> 
#>  .. @ axis_version_counter  :<environment: 0x559d2994d000> 
#>  .. @ vector_version_counter:<environment: 0x559d2994d2d8> 
#>  .. @ matrix_version_counter:<environment: 0x559d2994d5b0> 
chain <- complete_daf(new_dir, "r")
```
