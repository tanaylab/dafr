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
#>  @ internal              :<environment: 0x55c14cf2eb10> 
#>  @ cache                 :<environment: 0x55c14cf2dfe8> 
#>  @ axis_version_counter  :<environment: 0x55c14ceea278> 
#>  @ vector_version_counter:<environment: 0x55c14cee9fa0> 
#>  @ matrix_version_counter:<environment: 0x55c14cee9cc8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55c1533fdb58> 
#>  @ cache                 :<environment: 0x55c153f6a400> 
#>  @ axis_version_counter  :<environment: 0x55c153f688e0> 
#>  @ vector_version_counter:<environment: 0x55c153f6c438> 
#>  @ matrix_version_counter:<environment: 0x55c153f6c160> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55c153564830> 
#>  ..  ..@ cache                 :<environment: 0x55c1532be530> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55c1532bca10> 
#>  ..  ..@ vector_version_counter:<environment: 0x55c1532c0568> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55c1532c0290> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55c152fc40c0> 
#>  ..  ..@ cache                 :<environment: 0x55c152fc3598> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55c15178fac8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55c15178f7f0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55c15178f518> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55c152fc40c0> 
#>  .. @ cache                 :<environment: 0x55c152fc3598> 
#>  .. @ axis_version_counter  :<environment: 0x55c15178fac8> 
#>  .. @ vector_version_counter:<environment: 0x55c15178f7f0> 
#>  .. @ matrix_version_counter:<environment: 0x55c15178f518> 
chain <- complete_daf(new_dir, "r")
```
