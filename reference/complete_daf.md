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
#>  @ internal              :<environment: 0x55ad397eb508> 
#>  @ cache                 :<environment: 0x55ad397ee810> 
#>  @ axis_version_counter  :<environment: 0x55ad3d60ca90> 
#>  @ vector_version_counter:<environment: 0x55ad3d60c7b8> 
#>  @ matrix_version_counter:<environment: 0x55ad3d60c4e0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55ad3ebcef40> 
#>  @ cache                 :<environment: 0x55ad3ebcebf8> 
#>  @ axis_version_counter  :<environment: 0x55ad3ebd0f08> 
#>  @ vector_version_counter:<environment: 0x55ad3ebd0c30> 
#>  @ matrix_version_counter:<environment: 0x55ad3ebd0958> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55ad38e553a8> 
#>  ..  ..@ cache                 :<environment: 0x55ad405a4208> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55ad405a6518> 
#>  ..  ..@ vector_version_counter:<environment: 0x55ad405a6240> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55ad405a5f68> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55ad3f7ba648> 
#>  ..  ..@ cache                 :<environment: 0x55ad3f7bd950> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55ad3f7c1b80> 
#>  ..  ..@ vector_version_counter:<environment: 0x55ad3f7c18a8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55ad3f7c1598> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55ad3f7ba648> 
#>  .. @ cache                 :<environment: 0x55ad3f7bd950> 
#>  .. @ axis_version_counter  :<environment: 0x55ad3f7c1b80> 
#>  .. @ vector_version_counter:<environment: 0x55ad3f7c18a8> 
#>  .. @ matrix_version_counter:<environment: 0x55ad3f7c1598> 
chain <- complete_daf(new_dir, "r")
```
