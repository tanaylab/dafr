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
#>  @ internal              :<environment: 0x556809afa030> 
#>  @ cache                 :<environment: 0x556809aff258> 
#>  @ axis_version_counter  :<environment: 0x556809afd738> 
#>  @ vector_version_counter:<environment: 0x556809b01290> 
#>  @ matrix_version_counter:<environment: 0x556809b00fb8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x556802a2f858> 
#>  @ cache                 :<environment: 0x556802a2f510> 
#>  @ axis_version_counter  :<environment: 0x556802a31820> 
#>  @ vector_version_counter:<environment: 0x556802a31548> 
#>  @ matrix_version_counter:<environment: 0x556802a31270> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x556803ac1c40> 
#>  ..  ..@ cache                 :<environment: 0x556803803ab0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x556803805dc0> 
#>  ..  ..@ vector_version_counter:<environment: 0x556803805ae8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x556803805810> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x5568030b28e8> 
#>  ..  ..@ cache                 :<environment: 0x5568030b5bf0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5568030b7f00> 
#>  ..  ..@ vector_version_counter:<environment: 0x5568030b7c28> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5568030b7950> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x5568030b28e8> 
#>  .. @ cache                 :<environment: 0x5568030b5bf0> 
#>  .. @ axis_version_counter  :<environment: 0x5568030b7f00> 
#>  .. @ vector_version_counter:<environment: 0x5568030b7c28> 
#>  .. @ matrix_version_counter:<environment: 0x5568030b7950> 
chain <- complete_daf(new_dir, "r")
```
