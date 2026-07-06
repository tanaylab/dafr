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
#>  @ internal              :<environment: 0x55dd366d2068> 
#>  @ cache                 :<environment: 0x55dd366d1380> 
#>  @ axis_version_counter  :<environment: 0x55dd366d3690> 
#>  @ vector_version_counter:<environment: 0x55dd366d33b8> 
#>  @ matrix_version_counter:<environment: 0x55dd366d30e0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55dd346cb880> 
#>  @ cache                 :<environment: 0x55dd346cb538> 
#>  @ axis_version_counter  :<environment: 0x55dd346cd848> 
#>  @ vector_version_counter:<environment: 0x55dd346cd570> 
#>  @ matrix_version_counter:<environment: 0x55dd346cd298> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55dd33dd9ea8> 
#>  ..  ..@ cache                 :<environment: 0x55dd3484b280> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55dd34849728> 
#>  ..  ..@ vector_version_counter:<environment: 0x55dd3484d280> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55dd3484cfa8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55dd316834a8> 
#>  ..  ..@ cache                 :<environment: 0x55dd316827c0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55dd31684ad0> 
#>  ..  ..@ vector_version_counter:<environment: 0x55dd316847f8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55dd31684520> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55dd316834a8> 
#>  .. @ cache                 :<environment: 0x55dd316827c0> 
#>  .. @ axis_version_counter  :<environment: 0x55dd31684ad0> 
#>  .. @ vector_version_counter:<environment: 0x55dd316847f8> 
#>  .. @ matrix_version_counter:<environment: 0x55dd31684520> 
chain <- complete_daf(new_dir, "r")
```
