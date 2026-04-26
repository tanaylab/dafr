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
#>  @ internal              :<environment: 0x5573023ea838> 
#>  @ cache                 :<environment: 0x5573023e7530> 
#>  @ axis_version_counter  :<environment: 0x5573023e3530> 
#>  @ vector_version_counter:<environment: 0x5573023e3f40> 
#>  @ matrix_version_counter:<environment: 0x5573023e4448> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55730221e008> 
#>  @ cache                 :<environment: 0x55730221e350> 
#>  @ axis_version_counter  :<environment: 0x55730221fe70> 
#>  @ vector_version_counter:<environment: 0x55730221c318> 
#>  @ matrix_version_counter:<environment: 0x55730221c5f0> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x557302290fb0> 
#>  ..  ..@ cache                 :<environment: 0x55730227d320> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55730227b010> 
#>  ..  ..@ vector_version_counter:<environment: 0x55730227b2e8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55730227b5c0> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x557302303420> 
#>  ..  ..@ cache                 :<environment: 0x557302300150> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5573022fbf20> 
#>  ..  ..@ vector_version_counter:<environment: 0x5573022fc1f8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5573022fc4d0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x557302303420> 
#>  .. @ cache                 :<environment: 0x557302300150> 
#>  .. @ axis_version_counter  :<environment: 0x5573022fbf20> 
#>  .. @ vector_version_counter:<environment: 0x5573022fc1f8> 
#>  .. @ matrix_version_counter:<environment: 0x5573022fc4d0> 
chain <- complete_daf(new_dir, "r")
```
