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
#>  @ internal              :<environment: 0x55816c686a18> 
#>  @ cache                 :<environment: 0x55816c685ef0> 
#>  @ axis_version_counter  :<environment: 0x55816c560ed0> 
#>  @ vector_version_counter:<environment: 0x55816c560bf8> 
#>  @ matrix_version_counter:<environment: 0x55816c560920> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55816dba7590> 
#>  @ cache                 :<environment: 0x55816dba7248> 
#>  @ axis_version_counter  :<environment: 0x55816b380b48> 
#>  @ vector_version_counter:<environment: 0x55816b380870> 
#>  @ matrix_version_counter:<environment: 0x55816b380598> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55816934bab8> 
#>  ..  ..@ cache                 :<environment: 0x558169408bf8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5581694070d8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55816940ac30> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55816940a958> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55816cd0b0c8> 
#>  ..  ..@ cache                 :<environment: 0x55816cd0a5a0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55816cd0c8b0> 
#>  ..  ..@ vector_version_counter:<environment: 0x55816cd0c5d8> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55816cd0c300> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55816cd0b0c8> 
#>  .. @ cache                 :<environment: 0x55816cd0a5a0> 
#>  .. @ axis_version_counter  :<environment: 0x55816cd0c8b0> 
#>  .. @ vector_version_counter:<environment: 0x55816cd0c5d8> 
#>  .. @ matrix_version_counter:<environment: 0x55816cd0c300> 
chain <- complete_daf(new_dir, "r")
```
