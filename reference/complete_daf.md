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
#>  @ internal              :<environment: 0x563976cbe258> 
#>  @ cache                 :<environment: 0x563976cbed80> 
#>  @ axis_version_counter  :<environment: 0x563976cbca70> 
#>  @ vector_version_counter:<environment: 0x563976cbcd48> 
#>  @ matrix_version_counter:<environment: 0x563976cb91f0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x563976b06780> 
#>  @ cache                 :<environment: 0x563976b06ac8> 
#>  @ axis_version_counter  :<environment: 0x563976b047b8> 
#>  @ vector_version_counter:<environment: 0x563976b04a90> 
#>  @ matrix_version_counter:<environment: 0x563976b04d68> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x563976b7d5a0> 
#>  ..  ..@ cache                 :<environment: 0x563976b65b18> 
#>  ..  ..@ axis_version_counter  :<environment: 0x563976b63808> 
#>  ..  ..@ vector_version_counter:<environment: 0x563976b63ae0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x563976b63db8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x563976be5e10> 
#>  ..  ..@ cache                 :<environment: 0x563976be6938> 
#>  ..  ..@ axis_version_counter  :<environment: 0x563976be4628> 
#>  ..  ..@ vector_version_counter:<environment: 0x563976be4900> 
#>  ..  ..@ matrix_version_counter:<environment: 0x563976be4bd8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x563976be5e10> 
#>  .. @ cache                 :<environment: 0x563976be6938> 
#>  .. @ axis_version_counter  :<environment: 0x563976be4628> 
#>  .. @ vector_version_counter:<environment: 0x563976be4900> 
#>  .. @ matrix_version_counter:<environment: 0x563976be4bd8> 
chain <- complete_daf(new_dir, "r")
```
