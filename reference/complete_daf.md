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
#>  @ internal              :<environment: 0x563f30d6bc18> 
#>  @ cache                 :<environment: 0x563f30d6b0f0> 
#>  @ axis_version_counter  :<environment: 0x563f30d6d400> 
#>  @ vector_version_counter:<environment: 0x563f30d6d128> 
#>  @ matrix_version_counter:<environment: 0x563f30d6ce50> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x563f31ef8490> 
#>  @ cache                 :<environment: 0x563f31ef8148> 
#>  @ axis_version_counter  :<environment: 0x563f31efc340> 
#>  @ vector_version_counter:<environment: 0x563f31efc068> 
#>  @ matrix_version_counter:<environment: 0x563f31efbd90> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x563f308dd878> 
#>  ..  ..@ cache                 :<environment: 0x563f2ae5d158> 
#>  ..  ..@ axis_version_counter  :<environment: 0x563f2ae5f468> 
#>  ..  ..@ vector_version_counter:<environment: 0x563f2ae5f190> 
#>  ..  ..@ matrix_version_counter:<environment: 0x563f2ae5eeb8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x563f2e999578> 
#>  ..  ..@ cache                 :<environment: 0x563f2e998a50> 
#>  ..  ..@ axis_version_counter  :<environment: 0x563f2e99ad60> 
#>  ..  ..@ vector_version_counter:<environment: 0x563f2e99aa88> 
#>  ..  ..@ matrix_version_counter:<environment: 0x563f2e99e5e0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x563f2e999578> 
#>  .. @ cache                 :<environment: 0x563f2e998a50> 
#>  .. @ axis_version_counter  :<environment: 0x563f2e99ad60> 
#>  .. @ vector_version_counter:<environment: 0x563f2e99aa88> 
#>  .. @ matrix_version_counter:<environment: 0x563f2e99e5e0> 
chain <- complete_daf(new_dir, "r")
```
