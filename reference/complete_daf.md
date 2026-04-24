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
#>  @ internal              :<environment: 0x561238051838> 
#>  @ cache                 :<environment: 0x561238052360> 
#>  @ axis_version_counter  :<environment: 0x561238050050> 
#>  @ vector_version_counter:<environment: 0x561238050328> 
#>  @ matrix_version_counter:<environment: 0x561238050600> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x56123242f028> 
#>  @ cache                 :<environment: 0x56123242b578> 
#>  @ axis_version_counter  :<environment: 0x56123242d098> 
#>  @ vector_version_counter:<environment: 0x561232429540> 
#>  @ matrix_version_counter:<environment: 0x561232429818> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x5612324541c0> 
#>  ..  ..@ cache                 :<environment: 0x561232412810> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5612323e0990> 
#>  ..  ..@ vector_version_counter:<environment: 0x5612323e0c68> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5612323e0f40> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x561232972d98> 
#>  ..  ..@ cache                 :<environment: 0x5612329738c0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x561232971620> 
#>  ..  ..@ vector_version_counter:<environment: 0x5612329718f8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x561232971bd0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x561232972d98> 
#>  .. @ cache                 :<environment: 0x5612329738c0> 
#>  .. @ axis_version_counter  :<environment: 0x561232971620> 
#>  .. @ vector_version_counter:<environment: 0x5612329718f8> 
#>  .. @ matrix_version_counter:<environment: 0x561232971bd0> 
chain <- complete_daf(new_dir, "r")
```
