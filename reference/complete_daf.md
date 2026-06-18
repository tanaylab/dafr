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
#>  @ internal              :<environment: 0x560a1a7636d0> 
#>  @ cache                 :<environment: 0x560a1a7629e8> 
#>  @ axis_version_counter  :<environment: 0x560a1a764cf8> 
#>  @ vector_version_counter:<environment: 0x560a1a764a20> 
#>  @ matrix_version_counter:<environment: 0x560a1a764748> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x560a17807a68> 
#>  @ cache                 :<environment: 0x560a17807720> 
#>  @ axis_version_counter  :<environment: 0x560a17809a30> 
#>  @ vector_version_counter:<environment: 0x560a17809758> 
#>  @ matrix_version_counter:<environment: 0x560a17809480> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x560a1a11b4b0> 
#>  ..  ..@ cache                 :<environment: 0x560a1a1446b0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x560a1a142b90> 
#>  ..  ..@ vector_version_counter:<environment: 0x560a1b754228> 
#>  ..  ..@ matrix_version_counter:<environment: 0x560a1b753f50> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x560a19b2c660> 
#>  ..  ..@ cache                 :<environment: 0x560a19b2b978> 
#>  ..  ..@ axis_version_counter  :<environment: 0x560a1bee67d8> 
#>  ..  ..@ vector_version_counter:<environment: 0x560a1bee6500> 
#>  ..  ..@ matrix_version_counter:<environment: 0x560a1bee6228> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x560a19b2c660> 
#>  .. @ cache                 :<environment: 0x560a19b2b978> 
#>  .. @ axis_version_counter  :<environment: 0x560a1bee67d8> 
#>  .. @ vector_version_counter:<environment: 0x560a1bee6500> 
#>  .. @ matrix_version_counter:<environment: 0x560a1bee6228> 
chain <- complete_daf(new_dir, "r")
```
