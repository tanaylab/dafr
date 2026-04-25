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
#>  @ internal              :<environment: 0x557a3448a300> 
#>  @ cache                 :<environment: 0x557a34488a38> 
#>  @ axis_version_counter  :<environment: 0x557a34486728> 
#>  @ vector_version_counter:<environment: 0x557a34486a00> 
#>  @ matrix_version_counter:<environment: 0x557a34486cd8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x557a342c3820> 
#>  @ cache                 :<environment: 0x557a342bfd38> 
#>  @ axis_version_counter  :<environment: 0x557a342c1858> 
#>  @ vector_version_counter:<environment: 0x557a342c1b30> 
#>  @ matrix_version_counter:<environment: 0x557a342bc0b8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x557a34334870> 
#>  ..  ..@ cache                 :<environment: 0x557a3431cde8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x557a3431aad8> 
#>  ..  ..@ vector_version_counter:<environment: 0x557a3431adb0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x557a3431b088> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x557a343a4e68> 
#>  ..  ..@ cache                 :<environment: 0x557a3439fbe0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x557a343a1700> 
#>  ..  ..@ vector_version_counter:<environment: 0x557a3439dba8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x557a3439de80> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x557a343a4e68> 
#>  .. @ cache                 :<environment: 0x557a3439fbe0> 
#>  .. @ axis_version_counter  :<environment: 0x557a343a1700> 
#>  .. @ vector_version_counter:<environment: 0x557a3439dba8> 
#>  .. @ matrix_version_counter:<environment: 0x557a3439de80> 
chain <- complete_daf(new_dir, "r")
```
