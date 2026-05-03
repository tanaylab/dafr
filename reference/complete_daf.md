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
#>  @ internal              :<environment: 0x560f85fb66d8> 
#>  @ cache                 :<environment: 0x560f85fb5bb0> 
#>  @ axis_version_counter  :<environment: 0x560f85ef5640> 
#>  @ vector_version_counter:<environment: 0x560f85ef5368> 
#>  @ matrix_version_counter:<environment: 0x560f85ef5090> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x560f87315670> 
#>  @ cache                 :<environment: 0x560f87315328> 
#>  @ axis_version_counter  :<environment: 0x560f88625368> 
#>  @ vector_version_counter:<environment: 0x560f88625090> 
#>  @ matrix_version_counter:<environment: 0x560f88628be8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x560f7e44f678> 
#>  ..  ..@ cache                 :<environment: 0x560f84eb2e98> 
#>  ..  ..@ axis_version_counter  :<environment: 0x560f81bbc3e8> 
#>  ..  ..@ vector_version_counter:<environment: 0x560f81bbc110> 
#>  ..  ..@ matrix_version_counter:<environment: 0x560f81bbbe38> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x560f874f5ca8> 
#>  ..  ..@ cache                 :<environment: 0x560f874f5180> 
#>  ..  ..@ axis_version_counter  :<environment: 0x560f88582e40> 
#>  ..  ..@ vector_version_counter:<environment: 0x560f88582b68> 
#>  ..  ..@ matrix_version_counter:<environment: 0x560f88582890> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x560f874f5ca8> 
#>  .. @ cache                 :<environment: 0x560f874f5180> 
#>  .. @ axis_version_counter  :<environment: 0x560f88582e40> 
#>  .. @ vector_version_counter:<environment: 0x560f88582b68> 
#>  .. @ matrix_version_counter:<environment: 0x560f88582890> 
chain <- complete_daf(new_dir, "r")
```
