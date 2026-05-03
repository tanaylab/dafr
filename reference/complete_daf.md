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
#>  @ internal              :<environment: 0x5576a6e3ac38> 
#>  @ cache                 :<environment: 0x5576a6b4b320> 
#>  @ axis_version_counter  :<environment: 0x5576a6b4d630> 
#>  @ vector_version_counter:<environment: 0x5576a6b4d358> 
#>  @ matrix_version_counter:<environment: 0x5576a6b4d080> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x5576a417f850> 
#>  @ cache                 :<environment: 0x5576a417f508> 
#>  @ axis_version_counter  :<environment: 0x5576a4181818> 
#>  @ vector_version_counter:<environment: 0x5576a4181540> 
#>  @ matrix_version_counter:<environment: 0x5576a4181268> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x5576aa8b0ae8> 
#>  ..  ..@ cache                 :<environment: 0x5576aaf56f28> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5576aaf59238> 
#>  ..  ..@ vector_version_counter:<environment: 0x5576aaf58f60> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5576aaf58c88> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x5576a9901908> 
#>  ..  ..@ cache                 :<environment: 0x5576a9900de0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5576a99030f0> 
#>  ..  ..@ vector_version_counter:<environment: 0x5576a9902e18> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5576a9902b40> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x5576a9901908> 
#>  .. @ cache                 :<environment: 0x5576a9900de0> 
#>  .. @ axis_version_counter  :<environment: 0x5576a99030f0> 
#>  .. @ vector_version_counter:<environment: 0x5576a9902e18> 
#>  .. @ matrix_version_counter:<environment: 0x5576a9902b40> 
chain <- complete_daf(new_dir, "r")
```
