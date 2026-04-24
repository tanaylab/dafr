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
#>  @ internal              :<environment: 0x561dc8d5b3f8> 
#>  @ cache                 :<environment: 0x561dc8d5bf20> 
#>  @ axis_version_counter  :<environment: 0x561dc8d57cf0> 
#>  @ vector_version_counter:<environment: 0x561dc8d57fc8> 
#>  @ matrix_version_counter:<environment: 0x561dc8d582a0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x561dc3493090> 
#>  @ cache                 :<environment: 0x561dc34933d8> 
#>  @ axis_version_counter  :<environment: 0x561dc34910c8> 
#>  @ vector_version_counter:<environment: 0x561dc34913a0> 
#>  @ matrix_version_counter:<environment: 0x561dc348d848> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x561dc89bd648> 
#>  ..  ..@ cache                 :<environment: 0x561dc3522348> 
#>  ..  ..@ axis_version_counter  :<environment: 0x561dc3520038> 
#>  ..  ..@ vector_version_counter:<environment: 0x561dc3520310> 
#>  ..  ..@ matrix_version_counter:<environment: 0x561dc351c7b8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x561dc8b0b610> 
#>  ..  ..@ cache                 :<environment: 0x561dc8b08308> 
#>  ..  ..@ axis_version_counter  :<environment: 0x561dc8b033b8> 
#>  ..  ..@ vector_version_counter:<environment: 0x561dc8b03690> 
#>  ..  ..@ matrix_version_counter:<environment: 0x561dc8b03968> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x561dc8b0b610> 
#>  .. @ cache                 :<environment: 0x561dc8b08308> 
#>  .. @ axis_version_counter  :<environment: 0x561dc8b033b8> 
#>  .. @ vector_version_counter:<environment: 0x561dc8b03690> 
#>  .. @ matrix_version_counter:<environment: 0x561dc8b03968> 
chain <- complete_daf(new_dir, "r")
```
