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
#>  @ internal              :<environment: 0x562b21540b58> 
#>  @ cache                 :<environment: 0x562b21543e60> 
#>  @ axis_version_counter  :<environment: 0x562b21548090> 
#>  @ vector_version_counter:<environment: 0x562b21547db8> 
#>  @ matrix_version_counter:<environment: 0x562b21547ae0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x562b20f86400> 
#>  @ cache                 :<environment: 0x562b20f860b8> 
#>  @ axis_version_counter  :<environment: 0x562b20f883c8> 
#>  @ vector_version_counter:<environment: 0x562b20f880f0> 
#>  @ matrix_version_counter:<environment: 0x562b20f87e18> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x562b1e09cad8> 
#>  ..  ..@ cache                 :<environment: 0x562b2030cf88> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562b2030f298> 
#>  ..  ..@ vector_version_counter:<environment: 0x562b2030efc0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562b2030ece8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x562b210386f0> 
#>  ..  ..@ cache                 :<environment: 0x562b21037bc8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x562b21039ed8> 
#>  ..  ..@ vector_version_counter:<environment: 0x562b21039c00> 
#>  ..  ..@ matrix_version_counter:<environment: 0x562b21039928> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x562b210386f0> 
#>  .. @ cache                 :<environment: 0x562b21037bc8> 
#>  .. @ axis_version_counter  :<environment: 0x562b21039ed8> 
#>  .. @ vector_version_counter:<environment: 0x562b21039c00> 
#>  .. @ matrix_version_counter:<environment: 0x562b21039928> 
chain <- complete_daf(new_dir, "r")
```
