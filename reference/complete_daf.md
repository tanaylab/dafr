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
#>  @ internal              :<environment: 0x560953f3e150> 
#>  @ cache                 :<environment: 0x560953f3d628> 
#>  @ axis_version_counter  :<environment: 0x560953f14c18> 
#>  @ vector_version_counter:<environment: 0x560953f14940> 
#>  @ matrix_version_counter:<environment: 0x560953f14668> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x560959379548> 
#>  @ cache                 :<environment: 0x560959379200> 
#>  @ axis_version_counter  :<environment: 0x56095937b510> 
#>  @ vector_version_counter:<environment: 0x56095937b238> 
#>  @ matrix_version_counter:<environment: 0x56095937af60> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x560959a603b0> 
#>  ..  ..@ cache                 :<environment: 0x560958d1a660> 
#>  ..  ..@ axis_version_counter  :<environment: 0x560958d1c970> 
#>  ..  ..@ vector_version_counter:<environment: 0x560958d1c698> 
#>  ..  ..@ matrix_version_counter:<environment: 0x560958d1c3c0> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x560955824c30> 
#>  ..  ..@ cache                 :<environment: 0x560955824108> 
#>  ..  ..@ axis_version_counter  :<environment: 0x560957827e98> 
#>  ..  ..@ vector_version_counter:<environment: 0x560957827bc0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5609578278e8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x560955824c30> 
#>  .. @ cache                 :<environment: 0x560955824108> 
#>  .. @ axis_version_counter  :<environment: 0x560957827e98> 
#>  .. @ vector_version_counter:<environment: 0x560957827bc0> 
#>  .. @ matrix_version_counter:<environment: 0x5609578278e8> 
chain <- complete_daf(new_dir, "r")
```
