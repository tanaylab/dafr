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
#>  @ internal              :<environment: 0x55ecc37d1f38> 
#>  @ cache                 :<environment: 0x55ecc37d1250> 
#>  @ axis_version_counter  :<environment: 0x55ecc37d3560> 
#>  @ vector_version_counter:<environment: 0x55ecc37d3288> 
#>  @ matrix_version_counter:<environment: 0x55ecc37d2fb0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55ecc1a23798> 
#>  @ cache                 :<environment: 0x55ecc1a27280> 
#>  @ axis_version_counter  :<environment: 0x55ecc1a25760> 
#>  @ vector_version_counter:<environment: 0x55ecc1a292b8> 
#>  @ matrix_version_counter:<environment: 0x55ecc1a28fe0> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55ecbf0d2930> 
#>  ..  ..@ cache                 :<environment: 0x55ecb9121068> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55ecb9123298> 
#>  ..  ..@ vector_version_counter:<environment: 0x55ecb9122fc0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55ecb9122ce8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55ecc21f3ea0> 
#>  ..  ..@ cache                 :<environment: 0x55ecc21f6fe8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55ecc21f54c8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55ecc21f51f0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55ecc1b549f8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55ecc21f3ea0> 
#>  .. @ cache                 :<environment: 0x55ecc21f6fe8> 
#>  .. @ axis_version_counter  :<environment: 0x55ecc21f54c8> 
#>  .. @ vector_version_counter:<environment: 0x55ecc21f51f0> 
#>  .. @ matrix_version_counter:<environment: 0x55ecc1b549f8> 
chain <- complete_daf(new_dir, "r")
```
