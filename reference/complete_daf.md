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
#>  @ internal              :<environment: 0x55a0a9364ae0> 
#>  @ cache                 :<environment: 0x55a0a9365608> 
#>  @ axis_version_counter  :<environment: 0x55a0a93632f8> 
#>  @ vector_version_counter:<environment: 0x55a0a93635d0> 
#>  @ matrix_version_counter:<environment: 0x55a0a93638a8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55a0a452a5b8> 
#>  @ cache                 :<environment: 0x55a0a452a900> 
#>  @ axis_version_counter  :<environment: 0x55a0a45285f0> 
#>  @ vector_version_counter:<environment: 0x55a0a45288c8> 
#>  @ matrix_version_counter:<environment: 0x55a0a4528ba0> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55a0a4a3d778> 
#>  ..  ..@ cache                 :<environment: 0x55a0a41be4e8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55a0a41bc1d8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55a0a41bc4b0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55a0a41bc788> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55a0a3f38490> 
#>  ..  ..@ cache                 :<environment: 0x55a0a3f38fb8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55a0a3f36ca8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55a0a3f36f80> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55a0a3f37258> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55a0a3f38490> 
#>  .. @ cache                 :<environment: 0x55a0a3f38fb8> 
#>  .. @ axis_version_counter  :<environment: 0x55a0a3f36ca8> 
#>  .. @ vector_version_counter:<environment: 0x55a0a3f36f80> 
#>  .. @ matrix_version_counter:<environment: 0x55a0a3f37258> 
chain <- complete_daf(new_dir, "r")
```
