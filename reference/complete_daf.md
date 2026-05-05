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
#>  @ internal              :<environment: 0x563411b7b1f0> 
#>  @ cache                 :<environment: 0x563411b7a6c8> 
#>  @ axis_version_counter  :<environment: 0x563411a6c458> 
#>  @ vector_version_counter:<environment: 0x563411a6c180> 
#>  @ matrix_version_counter:<environment: 0x563411a6bea8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x563412802808> 
#>  @ cache                 :<environment: 0x5634128062f0> 
#>  @ axis_version_counter  :<environment: 0x5634128047d0> 
#>  @ vector_version_counter:<environment: 0x563412808328> 
#>  @ matrix_version_counter:<environment: 0x563412808050> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x56341544d180> 
#>  ..  ..@ cache                 :<environment: 0x563415afdda0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x563415afc280> 
#>  ..  ..@ vector_version_counter:<environment: 0x563415afbfa8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x56340f43ce60> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x56341611c8e0> 
#>  ..  ..@ cache                 :<environment: 0x56341611bdb8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x56340e8b9778> 
#>  ..  ..@ vector_version_counter:<environment: 0x56340e8b94a0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x56340e8b91c8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x56341611c8e0> 
#>  .. @ cache                 :<environment: 0x56341611bdb8> 
#>  .. @ axis_version_counter  :<environment: 0x56340e8b9778> 
#>  .. @ vector_version_counter:<environment: 0x56340e8b94a0> 
#>  .. @ matrix_version_counter:<environment: 0x56340e8b91c8> 
chain <- complete_daf(new_dir, "r")
```
