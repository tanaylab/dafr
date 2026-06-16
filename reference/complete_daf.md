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
#>  @ internal              :<environment: 0x55d1ad424588> 
#>  @ cache                 :<environment: 0x55d1ad45c0f0> 
#>  @ axis_version_counter  :<environment: 0x55d1ad45a5d0> 
#>  @ vector_version_counter:<environment: 0x55d1ad45e128> 
#>  @ matrix_version_counter:<environment: 0x55d1ad45de50> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55d1ad89f290> 
#>  @ cache                 :<environment: 0x55d1ad89ef48> 
#>  @ axis_version_counter  :<environment: 0x55d1ad8cde38> 
#>  @ vector_version_counter:<environment: 0x55d1ad8cdb60> 
#>  @ matrix_version_counter:<environment: 0x55d1ad8cd888> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55d1aba03b48> 
#>  ..  ..@ cache                 :<environment: 0x55d1b06163d8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55d1b06186e8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55d1b0618410> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55d1b0618138> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55d1addc07f8> 
#>  ..  ..@ cache                 :<environment: 0x55d1addbfcd0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55d1addc1fe0> 
#>  ..  ..@ vector_version_counter:<environment: 0x55d1addc1d08> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55d1addc5860> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55d1addc07f8> 
#>  .. @ cache                 :<environment: 0x55d1addbfcd0> 
#>  .. @ axis_version_counter  :<environment: 0x55d1addc1fe0> 
#>  .. @ vector_version_counter:<environment: 0x55d1addc1d08> 
#>  .. @ matrix_version_counter:<environment: 0x55d1addc5860> 
chain <- complete_daf(new_dir, "r")
```
