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
#>  @ internal              :<environment: 0x56428d65f380> 
#>  @ cache                 :<environment: 0x56428d65e858> 
#>  @ axis_version_counter  :<environment: 0x56428d660b68> 
#>  @ vector_version_counter:<environment: 0x56428d660890> 
#>  @ matrix_version_counter:<environment: 0x56428d6605b8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x56429d4a93e0> 
#>  @ cache                 :<environment: 0x56429d4a9098> 
#>  @ axis_version_counter  :<environment: 0x56429d4ab370> 
#>  @ vector_version_counter:<environment: 0x56429d4ab098> 
#>  @ matrix_version_counter:<environment: 0x56429d4aadc0> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x564297a318e0> 
#>  ..  ..@ cache                 :<environment: 0x56429774d6f8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x56429774fa08> 
#>  ..  ..@ vector_version_counter:<environment: 0x56429774f730> 
#>  ..  ..@ matrix_version_counter:<environment: 0x56429774f458> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x56429ac640e8> 
#>  ..  ..@ cache                 :<environment: 0x56429ac635c0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x56429ac658d0> 
#>  ..  ..@ vector_version_counter:<environment: 0x56429ac655c0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x56429ac652e8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x56429ac640e8> 
#>  .. @ cache                 :<environment: 0x56429ac635c0> 
#>  .. @ axis_version_counter  :<environment: 0x56429ac658d0> 
#>  .. @ vector_version_counter:<environment: 0x56429ac655c0> 
#>  .. @ matrix_version_counter:<environment: 0x56429ac652e8> 
chain <- complete_daf(new_dir, "r")
```
