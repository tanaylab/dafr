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
#>  @ internal              :<environment: 0x555cec0bf7c0> 
#>  @ cache                 :<environment: 0x555cec0c02e8> 
#>  @ axis_version_counter  :<environment: 0x555cec0bdfd8> 
#>  @ vector_version_counter:<environment: 0x555cec0be2b0> 
#>  @ matrix_version_counter:<environment: 0x555cec0be588> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x555cebf09c08> 
#>  @ cache                 :<environment: 0x555cebf09f50> 
#>  @ axis_version_counter  :<environment: 0x555cebf05d20> 
#>  @ vector_version_counter:<environment: 0x555cebf05ff8> 
#>  @ matrix_version_counter:<environment: 0x555cebf062d0> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x555cebf7eb08> 
#>  ..  ..@ cache                 :<environment: 0x555cebf67080> 
#>  ..  ..@ axis_version_counter  :<environment: 0x555cebf64d70> 
#>  ..  ..@ vector_version_counter:<environment: 0x555cebf65048> 
#>  ..  ..@ matrix_version_counter:<environment: 0x555cebf65320> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x555cebfe9298> 
#>  ..  ..@ cache                 :<environment: 0x555cebfe9dc0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x555cebfe5b90> 
#>  ..  ..@ vector_version_counter:<environment: 0x555cebfe5e68> 
#>  ..  ..@ matrix_version_counter:<environment: 0x555cebfe6140> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x555cebfe9298> 
#>  .. @ cache                 :<environment: 0x555cebfe9dc0> 
#>  .. @ axis_version_counter  :<environment: 0x555cebfe5b90> 
#>  .. @ vector_version_counter:<environment: 0x555cebfe5e68> 
#>  .. @ matrix_version_counter:<environment: 0x555cebfe6140> 
chain <- complete_daf(new_dir, "r")
```
