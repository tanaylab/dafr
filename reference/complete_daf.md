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
#>  @ internal              :<environment: 0x560696d5f0a0> 
#>  @ cache                 :<environment: 0x560696d642c8> 
#>  @ axis_version_counter  :<environment: 0x560696d665d8> 
#>  @ vector_version_counter:<environment: 0x560696d66300> 
#>  @ matrix_version_counter:<environment: 0x560696d66028> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x56069c2f9268> 
#>  @ cache                 :<environment: 0x56069c2f8f20> 
#>  @ axis_version_counter  :<environment: 0x56069c2fd150> 
#>  @ vector_version_counter:<environment: 0x56069c2fce78> 
#>  @ matrix_version_counter:<environment: 0x56069c2fcba0> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x56069cc14e10> 
#>  ..  ..@ cache                 :<environment: 0x56069c4e49d8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x56069c4e6ce8> 
#>  ..  ..@ vector_version_counter:<environment: 0x56069c4e6a10> 
#>  ..  ..@ matrix_version_counter:<environment: 0x56069c4e6738> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x56069cae3238> 
#>  ..  ..@ cache                 :<environment: 0x56069cae2710> 
#>  ..  ..@ axis_version_counter  :<environment: 0x56069cae6940> 
#>  ..  ..@ vector_version_counter:<environment: 0x56069cae6668> 
#>  ..  ..@ matrix_version_counter:<environment: 0x56069cae6390> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x56069cae3238> 
#>  .. @ cache                 :<environment: 0x56069cae2710> 
#>  .. @ axis_version_counter  :<environment: 0x56069cae6940> 
#>  .. @ vector_version_counter:<environment: 0x56069cae6668> 
#>  .. @ matrix_version_counter:<environment: 0x56069cae6390> 
chain <- complete_daf(new_dir, "r")
```
