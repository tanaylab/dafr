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
#>  @ internal              :<environment: 0x55d1e6f24348> 
#>  @ cache                 :<environment: 0x55d1e6f27650> 
#>  @ axis_version_counter  :<environment: 0x55d1e6f29960> 
#>  @ vector_version_counter:<environment: 0x55d1e6f29688> 
#>  @ matrix_version_counter:<environment: 0x55d1e6f293b0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55d1ed8c35c0> 
#>  @ cache                 :<environment: 0x55d1ed8c3278> 
#>  @ axis_version_counter  :<environment: 0x55d1ed46e9a8> 
#>  @ vector_version_counter:<environment: 0x55d1ed46e6d0> 
#>  @ matrix_version_counter:<environment: 0x55d1ed46e3f8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55d1eb52b820> 
#>  ..  ..@ cache                 :<environment: 0x55d1ed68d640> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55d1ed68f950> 
#>  ..  ..@ vector_version_counter:<environment: 0x55d1ed68f678> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55d1ed68f3a0> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55d1e82aaa68> 
#>  ..  ..@ cache                 :<environment: 0x55d1e82add70> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55d1e82b0080> 
#>  ..  ..@ vector_version_counter:<environment: 0x55d1e82afda8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55d1e82afad0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55d1e82aaa68> 
#>  .. @ cache                 :<environment: 0x55d1e82add70> 
#>  .. @ axis_version_counter  :<environment: 0x55d1e82b0080> 
#>  .. @ vector_version_counter:<environment: 0x55d1e82afda8> 
#>  .. @ matrix_version_counter:<environment: 0x55d1e82afad0> 
chain <- complete_daf(new_dir, "r")
```
