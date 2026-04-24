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
#>  @ internal              :<environment: 0x556bbbbc5da8> 
#>  @ cache                 :<environment: 0x556bbbbc68d0> 
#>  @ axis_version_counter  :<environment: 0x556bbbbc45c0> 
#>  @ vector_version_counter:<environment: 0x556bbbbc4898> 
#>  @ matrix_version_counter:<environment: 0x556bbbbc4b70> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x556bbcc658c0> 
#>  @ cache                 :<environment: 0x556bbcc626d0> 
#>  @ axis_version_counter  :<environment: 0x556bbcc5b4d0> 
#>  @ vector_version_counter:<environment: 0x556bbcc5c0a0> 
#>  @ matrix_version_counter:<environment: 0x556bbcc5a298> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x556bbc9cc388> 
#>  ..  ..@ cache                 :<environment: 0x556bbc9b4858> 
#>  ..  ..@ axis_version_counter  :<environment: 0x556bbc9b2548> 
#>  ..  ..@ vector_version_counter:<environment: 0x556bbc9b2890> 
#>  ..  ..@ matrix_version_counter:<environment: 0x556bbc9b2b68> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x556bbcfe7650> 
#>  ..  ..@ cache                 :<environment: 0x556bbcfe8178> 
#>  ..  ..@ axis_version_counter  :<environment: 0x556bbcfe5f10> 
#>  ..  ..@ vector_version_counter:<environment: 0x556bbcfe61e8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x556bbcfe64c0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x556bbcfe7650> 
#>  .. @ cache                 :<environment: 0x556bbcfe8178> 
#>  .. @ axis_version_counter  :<environment: 0x556bbcfe5f10> 
#>  .. @ vector_version_counter:<environment: 0x556bbcfe61e8> 
#>  .. @ matrix_version_counter:<environment: 0x556bbcfe64c0> 
chain <- complete_daf(new_dir, "r")
```
