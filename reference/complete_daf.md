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
#>  @ internal              :<environment: 0x56541a27a880> 
#>  @ cache                 :<environment: 0x56541aca7ea8> 
#>  @ axis_version_counter  :<environment: 0x56541aca5b98> 
#>  @ vector_version_counter:<environment: 0x56541aca5e70> 
#>  @ matrix_version_counter:<environment: 0x56541aca6148> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x56541566e948> 
#>  @ cache                 :<environment: 0x56541566ec90> 
#>  @ axis_version_counter  :<environment: 0x56541566c980> 
#>  @ vector_version_counter:<environment: 0x56541566cc58> 
#>  @ matrix_version_counter:<environment: 0x56541566cf30> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x5654196654d8> 
#>  ..  ..@ cache                 :<environment: 0x5654178f71b8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5654178f8cd8> 
#>  ..  ..@ vector_version_counter:<environment: 0x5654178f8fb0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5654178f5458> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x5654144bbee0> 
#>  ..  ..@ cache                 :<environment: 0x5654144b8bd8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5654144b68c8> 
#>  ..  ..@ vector_version_counter:<environment: 0x5654144b6ba0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5654144b6e78> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x5654144bbee0> 
#>  .. @ cache                 :<environment: 0x5654144b8bd8> 
#>  .. @ axis_version_counter  :<environment: 0x5654144b68c8> 
#>  .. @ vector_version_counter:<environment: 0x5654144b6ba0> 
#>  .. @ matrix_version_counter:<environment: 0x5654144b6e78> 
chain <- complete_daf(new_dir, "r")
```
