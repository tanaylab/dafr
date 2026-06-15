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
#>  @ internal              :<environment: 0x562b9bbcc870> 
#>  @ cache                 :<environment: 0x562b9bbcfb78> 
#>  @ axis_version_counter  :<environment: 0x562b9bbd1e88> 
#>  @ vector_version_counter:<environment: 0x562b9bbd1bb0> 
#>  @ matrix_version_counter:<environment: 0x562b9bbd18d8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x562ba2a8abb8> 
#>  @ cache                 :<environment: 0x562ba2a8a870> 
#>  @ axis_version_counter  :<environment: 0x562ba2a8cb80> 
#>  @ vector_version_counter:<environment: 0x562ba2a8c8a8> 
#>  @ matrix_version_counter:<environment: 0x562ba2a8c5d0> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x562b9ccbe4f0> 
#>  ..  ..@ cache                 :<environment: 0x562b9f4a7880> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562b9bd3c0f0> 
#>  ..  ..@ vector_version_counter:<environment: 0x562b9bd3be18> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562b9bd3bb40> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x562b9af08640> 
#>  ..  ..@ cache                 :<environment: 0x562b9b922758> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562b9b920c38> 
#>  ..  ..@ vector_version_counter:<environment: 0x562b9b924790> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562b9b9244b8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x562b9af08640> 
#>  .. @ cache                 :<environment: 0x562b9b922758> 
#>  .. @ axis_version_counter  :<environment: 0x562b9b920c38> 
#>  .. @ vector_version_counter:<environment: 0x562b9b924790> 
#>  .. @ matrix_version_counter:<environment: 0x562b9b9244b8> 
chain <- complete_daf(new_dir, "r")
```
