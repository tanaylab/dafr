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
#>  @ internal              :<environment: 0x562a027178c8> 
#>  @ cache                 :<environment: 0x562a026f5c70> 
#>  @ axis_version_counter  :<environment: 0x562a026f7f80> 
#>  @ vector_version_counter:<environment: 0x562a026f7ca8> 
#>  @ matrix_version_counter:<environment: 0x562a026f79d0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x562a052bdce0> 
#>  @ cache                 :<environment: 0x562a052bd998> 
#>  @ axis_version_counter  :<environment: 0x562a052bfca8> 
#>  @ vector_version_counter:<environment: 0x562a052bf9d0> 
#>  @ matrix_version_counter:<environment: 0x562a052bf6f8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x5629fc219da0> 
#>  ..  ..@ cache                 :<environment: 0x562a05bc0b20> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562a05bc2e30> 
#>  ..  ..@ vector_version_counter:<environment: 0x562a05bc2b58> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562a05bc2880> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x562a06157070> 
#>  ..  ..@ cache                 :<environment: 0x562a06156548> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562a06158858> 
#>  ..  ..@ vector_version_counter:<environment: 0x562a06158580> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562a061582a8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x562a06157070> 
#>  .. @ cache                 :<environment: 0x562a06156548> 
#>  .. @ axis_version_counter  :<environment: 0x562a06158858> 
#>  .. @ vector_version_counter:<environment: 0x562a06158580> 
#>  .. @ matrix_version_counter:<environment: 0x562a061582a8> 
chain <- complete_daf(new_dir, "r")
```
