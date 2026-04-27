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
#>  @ internal              :<environment: 0x563ac51f7b58> 
#>  @ cache                 :<environment: 0x563ac51f4850> 
#>  @ axis_version_counter  :<environment: 0x563ac51f2540> 
#>  @ vector_version_counter:<environment: 0x563ac51f2818> 
#>  @ matrix_version_counter:<environment: 0x563ac51f2af0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x563ac502b2f0> 
#>  @ cache                 :<environment: 0x563ac502b638> 
#>  @ axis_version_counter  :<environment: 0x563ac5029328> 
#>  @ vector_version_counter:<environment: 0x563ac5029600> 
#>  @ matrix_version_counter:<environment: 0x563ac50298d8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x563ac509e298> 
#>  ..  ..@ cache                 :<environment: 0x563ac50867d8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x563ac50882f8> 
#>  ..  ..@ vector_version_counter:<environment: 0x563ac50847a0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x563ac5084a78> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x563ac510c8d8> 
#>  ..  ..@ cache                 :<environment: 0x563ac510d400> 
#>  ..  ..@ axis_version_counter  :<environment: 0x563ac510b128> 
#>  ..  ..@ vector_version_counter:<environment: 0x563ac510b400> 
#>  ..  ..@ matrix_version_counter:<environment: 0x563ac510b6d8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x563ac510c8d8> 
#>  .. @ cache                 :<environment: 0x563ac510d400> 
#>  .. @ axis_version_counter  :<environment: 0x563ac510b128> 
#>  .. @ vector_version_counter:<environment: 0x563ac510b400> 
#>  .. @ matrix_version_counter:<environment: 0x563ac510b6d8> 
chain <- complete_daf(new_dir, "r")
```
