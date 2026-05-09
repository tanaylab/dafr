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
#>  @ internal              :<environment: 0x563538ec9658> 
#>  @ cache                 :<environment: 0x563538ec8b30> 
#>  @ axis_version_counter  :<environment: 0x563538ecae40> 
#>  @ vector_version_counter:<environment: 0x563538ecab68> 
#>  @ matrix_version_counter:<environment: 0x563538eca890> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x56353a544108> 
#>  @ cache                 :<environment: 0x56353a543dc0> 
#>  @ axis_version_counter  :<environment: 0x56353a5460d0> 
#>  @ vector_version_counter:<environment: 0x56353a545df8> 
#>  @ matrix_version_counter:<environment: 0x56353a545b20> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x56353dc23e20> 
#>  ..  ..@ cache                 :<environment: 0x56353d6c4358> 
#>  ..  ..@ axis_version_counter  :<environment: 0x56353d6c6668> 
#>  ..  ..@ vector_version_counter:<environment: 0x56353d6c6390> 
#>  ..  ..@ matrix_version_counter:<environment: 0x56353d6c60b8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x56353c31aa38> 
#>  ..  ..@ cache                 :<environment: 0x56353c319f10> 
#>  ..  ..@ axis_version_counter  :<environment: 0x56353c31c220> 
#>  ..  ..@ vector_version_counter:<environment: 0x56353c31bf48> 
#>  ..  ..@ matrix_version_counter:<environment: 0x56353c3219c0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x56353c31aa38> 
#>  .. @ cache                 :<environment: 0x56353c319f10> 
#>  .. @ axis_version_counter  :<environment: 0x56353c31c220> 
#>  .. @ vector_version_counter:<environment: 0x56353c31bf48> 
#>  .. @ matrix_version_counter:<environment: 0x56353c3219c0> 
chain <- complete_daf(new_dir, "r")
```
