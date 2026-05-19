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
#>  @ internal              :<environment: 0x55a5dcbce188> 
#>  @ cache                 :<environment: 0x55a5dcbcd660> 
#>  @ axis_version_counter  :<environment: 0x55a5dcbcf970> 
#>  @ vector_version_counter:<environment: 0x55a5dcbcf698> 
#>  @ matrix_version_counter:<environment: 0x55a5dcbcf3c0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55a5dc9e0cb0> 
#>  @ cache                 :<environment: 0x55a5dc9e0968> 
#>  @ axis_version_counter  :<environment: 0x55a5dc9e2c78> 
#>  @ vector_version_counter:<environment: 0x55a5dc9e29a0> 
#>  @ matrix_version_counter:<environment: 0x55a5dc9e26c8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55a5d6998398> 
#>  ..  ..@ cache                 :<environment: 0x55a5d69afec8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55a5d69b21d8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55a5d69b1f00> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55a5d69b1c28> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55a5dc460920> 
#>  ..  ..@ cache                 :<environment: 0x55a5dc45fdf8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55a5dc462108> 
#>  ..  ..@ vector_version_counter:<environment: 0x55a5dc461e30> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55a5dc461b58> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55a5dc460920> 
#>  .. @ cache                 :<environment: 0x55a5dc45fdf8> 
#>  .. @ axis_version_counter  :<environment: 0x55a5dc462108> 
#>  .. @ vector_version_counter:<environment: 0x55a5dc461e30> 
#>  .. @ matrix_version_counter:<environment: 0x55a5dc461b58> 
chain <- complete_daf(new_dir, "r")
```
