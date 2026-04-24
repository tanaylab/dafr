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
#>  @ internal              :<environment: 0x55dd57f34af0> 
#>  @ cache                 :<environment: 0x55dd57f35618> 
#>  @ axis_version_counter  :<environment: 0x55dd57f33308> 
#>  @ vector_version_counter:<environment: 0x55dd57f335e0> 
#>  @ matrix_version_counter:<environment: 0x55dd57f338b8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55dd52660278> 
#>  @ cache                 :<environment: 0x55dd526605c0> 
#>  @ axis_version_counter  :<environment: 0x55dd5265e2b0> 
#>  @ vector_version_counter:<environment: 0x55dd5265e588> 
#>  @ matrix_version_counter:<environment: 0x55dd5265e860> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55dd527721c8> 
#>  ..  ..@ cache                 :<environment: 0x55dd5272e8f8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55dd526fe998> 
#>  ..  ..@ vector_version_counter:<environment: 0x55dd526fec70> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55dd526fef48> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55dd57cb7bf0> 
#>  ..  ..@ cache                 :<environment: 0x55dd57cb8718> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55dd57cb6408> 
#>  ..  ..@ vector_version_counter:<environment: 0x55dd57cb66e0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55dd57cb69b8> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55dd57cb7bf0> 
#>  .. @ cache                 :<environment: 0x55dd57cb8718> 
#>  .. @ axis_version_counter  :<environment: 0x55dd57cb6408> 
#>  .. @ vector_version_counter:<environment: 0x55dd57cb66e0> 
#>  .. @ matrix_version_counter:<environment: 0x55dd57cb69b8> 
chain <- complete_daf(new_dir, "r")
```
