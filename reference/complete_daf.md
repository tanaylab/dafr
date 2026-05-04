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
#>  @ internal              :<environment: 0x560daf3e4370> 
#>  @ cache                 :<environment: 0x560daf226588> 
#>  @ axis_version_counter  :<environment: 0x560daf228898> 
#>  @ vector_version_counter:<environment: 0x560daf2285c0> 
#>  @ matrix_version_counter:<environment: 0x560daf2282e8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x560db09bb038> 
#>  @ cache                 :<environment: 0x560db09bacf0> 
#>  @ axis_version_counter  :<environment: 0x560db09bd000> 
#>  @ vector_version_counter:<environment: 0x560db09bcd28> 
#>  @ matrix_version_counter:<environment: 0x560db09bca50> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x560daf28f2c8> 
#>  ..  ..@ cache                 :<environment: 0x560db17ab9a8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x560db17adcb8> 
#>  ..  ..@ vector_version_counter:<environment: 0x560db17ad9e0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x560db17ad708> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x560daaa292e8> 
#>  ..  ..@ cache                 :<environment: 0x560daaa287c0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x560daaa2aad0> 
#>  ..  ..@ vector_version_counter:<environment: 0x560daaa2a7f8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x560daaa2a520> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x560daaa292e8> 
#>  .. @ cache                 :<environment: 0x560daaa287c0> 
#>  .. @ axis_version_counter  :<environment: 0x560daaa2aad0> 
#>  .. @ vector_version_counter:<environment: 0x560daaa2a7f8> 
#>  .. @ matrix_version_counter:<environment: 0x560daaa2a520> 
chain <- complete_daf(new_dir, "r")
```
