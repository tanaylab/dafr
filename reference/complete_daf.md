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
#>  @ internal              :<environment: 0x55c4e7616c70> 
#>  @ cache                 :<environment: 0x55c4e7619db8> 
#>  @ axis_version_counter  :<environment: 0x55c4e761c0c8> 
#>  @ vector_version_counter:<environment: 0x55c4e761bdf0> 
#>  @ matrix_version_counter:<environment: 0x55c4e761bb18> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55c4e3c5f340> 
#>  @ cache                 :<environment: 0x55c4e3c5eff8> 
#>  @ axis_version_counter  :<environment: 0x55c4e3c61298> 
#>  @ vector_version_counter:<environment: 0x55c4e3c60fc0> 
#>  @ matrix_version_counter:<environment: 0x55c4e3c60ce8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55c4e79d60e0> 
#>  ..  ..@ cache                 :<environment: 0x55c4e8590590> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55c4e85928a0> 
#>  ..  ..@ vector_version_counter:<environment: 0x55c4e85925c8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55c4e85922f0> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55c4e55ba418> 
#>  ..  ..@ cache                 :<environment: 0x55c4e55b9730> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55c4e55bba40> 
#>  ..  ..@ vector_version_counter:<environment: 0x55c4e55bb768> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55c4e55bb490> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55c4e55ba418> 
#>  .. @ cache                 :<environment: 0x55c4e55b9730> 
#>  .. @ axis_version_counter  :<environment: 0x55c4e55bba40> 
#>  .. @ vector_version_counter:<environment: 0x55c4e55bb768> 
#>  .. @ matrix_version_counter:<environment: 0x55c4e55bb490> 
chain <- complete_daf(new_dir, "r")
```
