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
#>  @ internal              :<environment: 0x55b01533fc08> 
#>  @ cache                 :<environment: 0x55b01a536fb0> 
#>  @ axis_version_counter  :<environment: 0x55b01a5392c0> 
#>  @ vector_version_counter:<environment: 0x55b01a538fe8> 
#>  @ matrix_version_counter:<environment: 0x55b01a538d10> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55b01d961e50> 
#>  @ cache                 :<environment: 0x55b01d961b08> 
#>  @ axis_version_counter  :<environment: 0x55b01d963e18> 
#>  @ vector_version_counter:<environment: 0x55b01d963b40> 
#>  @ matrix_version_counter:<environment: 0x55b01d963868> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55b01ccf9938> 
#>  ..  ..@ cache                 :<environment: 0x55b01b5806b8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55b01b5829c8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55b01b5826f0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55b01b582418> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55b019d2b2c8> 
#>  ..  ..@ cache                 :<environment: 0x55b019d0c460> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55b019d0e770> 
#>  ..  ..@ vector_version_counter:<environment: 0x55b019d0e498> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55b019d0e1c0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55b019d2b2c8> 
#>  .. @ cache                 :<environment: 0x55b019d0c460> 
#>  .. @ axis_version_counter  :<environment: 0x55b019d0e770> 
#>  .. @ vector_version_counter:<environment: 0x55b019d0e498> 
#>  .. @ matrix_version_counter:<environment: 0x55b019d0e1c0> 
chain <- complete_daf(new_dir, "r")
```
