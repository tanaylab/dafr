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
#>  @ internal              :<environment: 0x55c9900ad608> 
#>  @ cache                 :<environment: 0x55c9900b0910> 
#>  @ axis_version_counter  :<environment: 0x55c9900b2c20> 
#>  @ vector_version_counter:<environment: 0x55c9900b2948> 
#>  @ matrix_version_counter:<environment: 0x55c9900b2670> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55c992ae8940> 
#>  @ cache                 :<environment: 0x55c992ae85f8> 
#>  @ axis_version_counter  :<environment: 0x55c992aea908> 
#>  @ vector_version_counter:<environment: 0x55c992aea630> 
#>  @ matrix_version_counter:<environment: 0x55c992aea358> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55c990c21298> 
#>  ..  ..@ cache                 :<environment: 0x55c9909f67d8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55c9909f8ae8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55c9909f8810> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55c9909f8538> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55c991826500> 
#>  ..  ..@ cache                 :<environment: 0x55c991829808> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55c991827ce8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55c991827a10> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55c99182b568> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55c991826500> 
#>  .. @ cache                 :<environment: 0x55c991829808> 
#>  .. @ axis_version_counter  :<environment: 0x55c991827ce8> 
#>  .. @ vector_version_counter:<environment: 0x55c991827a10> 
#>  .. @ matrix_version_counter:<environment: 0x55c99182b568> 
chain <- complete_daf(new_dir, "r")
```
