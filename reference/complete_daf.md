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
#>  @ internal              :<environment: 0x562ba3540a78> 
#>  @ cache                 :<environment: 0x562ba30d1d60> 
#>  @ axis_version_counter  :<environment: 0x562ba30d4070> 
#>  @ vector_version_counter:<environment: 0x562ba30d3d98> 
#>  @ matrix_version_counter:<environment: 0x562ba30d3ac0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x562ba3554a40> 
#>  @ cache                 :<environment: 0x562ba35546f8> 
#>  @ axis_version_counter  :<environment: 0x562ba323ee28> 
#>  @ vector_version_counter:<environment: 0x562ba323eb50> 
#>  @ matrix_version_counter:<environment: 0x562ba323e878> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x562ba7186560> 
#>  ..  ..@ cache                 :<environment: 0x562b9bbe9340> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562b9bbeb650> 
#>  ..  ..@ vector_version_counter:<environment: 0x562b9bbeb378> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562b9bbeb0a0> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x562ba0869400> 
#>  ..  ..@ cache                 :<environment: 0x562ba08688d8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562ba086abe8> 
#>  ..  ..@ vector_version_counter:<environment: 0x562ba086a910> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562ba086a638> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x562ba0869400> 
#>  .. @ cache                 :<environment: 0x562ba08688d8> 
#>  .. @ axis_version_counter  :<environment: 0x562ba086abe8> 
#>  .. @ vector_version_counter:<environment: 0x562ba086a910> 
#>  .. @ matrix_version_counter:<environment: 0x562ba086a638> 
chain <- complete_daf(new_dir, "r")
```
