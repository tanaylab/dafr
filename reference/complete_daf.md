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
#>  @ internal              :<environment: 0x55fc0ea21bb0> 
#>  @ cache                 :<environment: 0x55fc0ea21088> 
#>  @ axis_version_counter  :<environment: 0x55fc0ea23398> 
#>  @ vector_version_counter:<environment: 0x55fc0ea230c0> 
#>  @ matrix_version_counter:<environment: 0x55fc0ea22de8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55fc0905be78> 
#>  @ cache                 :<environment: 0x55fc0905bb30> 
#>  @ axis_version_counter  :<environment: 0x55fc09069ec8> 
#>  @ vector_version_counter:<environment: 0x55fc09069bf0> 
#>  @ matrix_version_counter:<environment: 0x55fc09069918> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55fc09d87e90> 
#>  ..  ..@ cache                 :<environment: 0x55fc09d9f9f8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55fc09da3c88> 
#>  ..  ..@ vector_version_counter:<environment: 0x55fc09da39b0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55fc09da36d8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55fc0ec477f8> 
#>  ..  ..@ cache                 :<environment: 0x55fc0ec46cd0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55fc0ec4af00> 
#>  ..  ..@ vector_version_counter:<environment: 0x55fc0ec4ac28> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55fc0ec4a950> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55fc0ec477f8> 
#>  .. @ cache                 :<environment: 0x55fc0ec46cd0> 
#>  .. @ axis_version_counter  :<environment: 0x55fc0ec4af00> 
#>  .. @ vector_version_counter:<environment: 0x55fc0ec4ac28> 
#>  .. @ matrix_version_counter:<environment: 0x55fc0ec4a950> 
chain <- complete_daf(new_dir, "r")
```
