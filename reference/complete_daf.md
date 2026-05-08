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
#>  @ internal              :<environment: 0x558acf822360> 
#>  @ cache                 :<environment: 0x558acf821838> 
#>  @ axis_version_counter  :<environment: 0x558acf823ad8> 
#>  @ vector_version_counter:<environment: 0x558acf823800> 
#>  @ matrix_version_counter:<environment: 0x558acf7ae998> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x558ad0461740> 
#>  @ cache                 :<environment: 0x558ad04613f8> 
#>  @ axis_version_counter  :<environment: 0x558ad0463708> 
#>  @ vector_version_counter:<environment: 0x558ad0463430> 
#>  @ matrix_version_counter:<environment: 0x558ad0463158> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x558aca3c0598> 
#>  ..  ..@ cache                 :<environment: 0x558aca3d9fe8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x558aca3dc2f8> 
#>  ..  ..@ vector_version_counter:<environment: 0x558aca3dc020> 
#>  ..  ..@ matrix_version_counter:<environment: 0x558aca3dbd48> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x558ac9287378> 
#>  ..  ..@ cache                 :<environment: 0x558ac928a680> 
#>  ..  ..@ axis_version_counter  :<environment: 0x558ac928c958> 
#>  ..  ..@ vector_version_counter:<environment: 0x558ac928c680> 
#>  ..  ..@ matrix_version_counter:<environment: 0x558ac928c3a8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x558ac9287378> 
#>  .. @ cache                 :<environment: 0x558ac928a680> 
#>  .. @ axis_version_counter  :<environment: 0x558ac928c958> 
#>  .. @ vector_version_counter:<environment: 0x558ac928c680> 
#>  .. @ matrix_version_counter:<environment: 0x558ac928c3a8> 
chain <- complete_daf(new_dir, "r")
```
