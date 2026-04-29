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
#>  @ internal              :<environment: 0x55efb22a5150> 
#>  @ cache                 :<environment: 0x55efb22a5d90> 
#>  @ axis_version_counter  :<environment: 0x55efb229abd8> 
#>  @ vector_version_counter:<environment: 0x55efb229aeb0> 
#>  @ matrix_version_counter:<environment: 0x55efb229b188> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55efb20e1100> 
#>  @ cache                 :<environment: 0x55efb20e1448> 
#>  @ axis_version_counter  :<environment: 0x55efb20df138> 
#>  @ vector_version_counter:<environment: 0x55efb20df410> 
#>  @ matrix_version_counter:<environment: 0x55efb20df6e8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55efb21540e0> 
#>  ..  ..@ cache                 :<environment: 0x55efb2140418> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55efb213e108> 
#>  ..  ..@ vector_version_counter:<environment: 0x55efb213e3e0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55efb213a888> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55efb21c6550> 
#>  ..  ..@ cache                 :<environment: 0x55efb21c3248> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55efb21c0f38> 
#>  ..  ..@ vector_version_counter:<environment: 0x55efb21c1210> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55efb21c14e8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55efb21c6550> 
#>  .. @ cache                 :<environment: 0x55efb21c3248> 
#>  .. @ axis_version_counter  :<environment: 0x55efb21c0f38> 
#>  .. @ vector_version_counter:<environment: 0x55efb21c1210> 
#>  .. @ matrix_version_counter:<environment: 0x55efb21c14e8> 
chain <- complete_daf(new_dir, "r")
```
