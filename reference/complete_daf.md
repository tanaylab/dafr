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
#>  @ internal              :<environment: 0x560825c8d1e0> 
#>  @ cache                 :<environment: 0x560825c904e8> 
#>  @ axis_version_counter  :<environment: 0x56081c104218> 
#>  @ vector_version_counter:<environment: 0x56081c103f40> 
#>  @ matrix_version_counter:<environment: 0x56081c103c68> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x5608236657b8> 
#>  @ cache                 :<environment: 0x560823665470> 
#>  @ axis_version_counter  :<environment: 0x560823667780> 
#>  @ vector_version_counter:<environment: 0x5608236674a8> 
#>  @ matrix_version_counter:<environment: 0x5608236671d0> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x56081fa53e20> 
#>  ..  ..@ cache                 :<environment: 0x5608262252d0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x560825f4d5a0> 
#>  ..  ..@ vector_version_counter:<environment: 0x560825f4d2c8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x560825f4cff0> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x560826a16650> 
#>  ..  ..@ cache                 :<environment: 0x560826a15b28> 
#>  ..  ..@ axis_version_counter  :<environment: 0x56081f9df408> 
#>  ..  ..@ vector_version_counter:<environment: 0x56081f9df130> 
#>  ..  ..@ matrix_version_counter:<environment: 0x56081f9dee58> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x560826a16650> 
#>  .. @ cache                 :<environment: 0x560826a15b28> 
#>  .. @ axis_version_counter  :<environment: 0x56081f9df408> 
#>  .. @ vector_version_counter:<environment: 0x56081f9df130> 
#>  .. @ matrix_version_counter:<environment: 0x56081f9dee58> 
chain <- complete_daf(new_dir, "r")
```
