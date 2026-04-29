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
#>  @ internal              :<environment: 0x5570c150dfe8> 
#>  @ cache                 :<environment: 0x5570c150eb10> 
#>  @ axis_version_counter  :<environment: 0x5570c150c800> 
#>  @ vector_version_counter:<environment: 0x5570c150cad8> 
#>  @ matrix_version_counter:<environment: 0x5570c150cdb0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x5570c1356510> 
#>  @ cache                 :<environment: 0x5570c1356858> 
#>  @ axis_version_counter  :<environment: 0x5570c1354548> 
#>  @ vector_version_counter:<environment: 0x5570c1354820> 
#>  @ matrix_version_counter:<environment: 0x5570c1354af8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x5570c13cb410> 
#>  ..  ..@ cache                 :<environment: 0x5570c13b38e0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5570c13b5400> 
#>  ..  ..@ vector_version_counter:<environment: 0x5570c13b56d8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5570c13b1b80> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x5570c143b8f0> 
#>  ..  ..@ cache                 :<environment: 0x5570c14385e8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5570c14362d8> 
#>  ..  ..@ vector_version_counter:<environment: 0x5570c14365b0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5570c1436888> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x5570c143b8f0> 
#>  .. @ cache                 :<environment: 0x5570c14385e8> 
#>  .. @ axis_version_counter  :<environment: 0x5570c14362d8> 
#>  .. @ vector_version_counter:<environment: 0x5570c14365b0> 
#>  .. @ matrix_version_counter:<environment: 0x5570c1436888> 
chain <- complete_daf(new_dir, "r")
```
