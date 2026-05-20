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
#>  @ internal              :<environment: 0x562b25a64920> 
#>  @ cache                 :<environment: 0x562b25a67c28> 
#>  @ axis_version_counter  :<environment: 0x562b25a69f38> 
#>  @ vector_version_counter:<environment: 0x562b25a69c60> 
#>  @ matrix_version_counter:<environment: 0x562b25a69988> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x562b23b79c90> 
#>  @ cache                 :<environment: 0x562b23b79948> 
#>  @ axis_version_counter  :<environment: 0x562b23b7bc58> 
#>  @ vector_version_counter:<environment: 0x562b23b7b980> 
#>  @ matrix_version_counter:<environment: 0x562b23b7b6a8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x562b23d8d960> 
#>  ..  ..@ cache                 :<environment: 0x562b23da54c8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562b23da77d8> 
#>  ..  ..@ vector_version_counter:<environment: 0x562b23da7500> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562b23da7228> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x562b235f2f58> 
#>  ..  ..@ cache                 :<environment: 0x562b235f6260> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562b235f8570> 
#>  ..  ..@ vector_version_counter:<environment: 0x562b235f8298> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562b235f7fc0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x562b235f2f58> 
#>  .. @ cache                 :<environment: 0x562b235f6260> 
#>  .. @ axis_version_counter  :<environment: 0x562b235f8570> 
#>  .. @ vector_version_counter:<environment: 0x562b235f8298> 
#>  .. @ matrix_version_counter:<environment: 0x562b235f7fc0> 
chain <- complete_daf(new_dir, "r")
```
