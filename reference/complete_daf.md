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
#>  @ internal              :<environment: 0x55954ddf81f0> 
#>  @ cache                 :<environment: 0x55954ddf8d50> 
#>  @ axis_version_counter  :<environment: 0x55954ddf6a78> 
#>  @ vector_version_counter:<environment: 0x55954ddf6d50> 
#>  @ matrix_version_counter:<environment: 0x55954ddf7028> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55954dc32660> 
#>  @ cache                 :<environment: 0x55954dc329a8> 
#>  @ axis_version_counter  :<environment: 0x55954dc30698> 
#>  @ vector_version_counter:<environment: 0x55954dc30970> 
#>  @ matrix_version_counter:<environment: 0x55954dc30c48> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55954dca9170> 
#>  ..  ..@ cache                 :<environment: 0x55954dc95470> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55954dc93160> 
#>  ..  ..@ vector_version_counter:<environment: 0x55954dc93438> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55954dc93710> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55954dd1d228> 
#>  ..  ..@ cache                 :<environment: 0x55954dd19f20> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55954dd1ba40> 
#>  ..  ..@ vector_version_counter:<environment: 0x55954dd17ee8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55954dd181c0> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55954dd1d228> 
#>  .. @ cache                 :<environment: 0x55954dd19f20> 
#>  .. @ axis_version_counter  :<environment: 0x55954dd1ba40> 
#>  .. @ vector_version_counter:<environment: 0x55954dd17ee8> 
#>  .. @ matrix_version_counter:<environment: 0x55954dd181c0> 
chain <- complete_daf(new_dir, "r")
```
