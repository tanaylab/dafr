# Reopen a persistent chain from disk.

Opens the complete chain of repositories rooted at `leaf` by following
the `base_daf_repository` scalar of each. Returns a
[`chain_reader()`](https://tanaylab.github.io/dafr/reference/chain_reader.md)
(`mode = "r"`) or
[`chain_writer()`](https://tanaylab.github.io/dafr/reference/chain_writer.md)
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

## Details

A repository records only its own immediate bases; what each of them
rests on is recorded in it. A repository reached through more than one
of them appears once, at its earliest position, so that a base never
overrides what rests on it.

## Examples

``` r
tmp_root <- tempfile(); dir.create(tmp_root)
base_dir <- file.path(tmp_root, "base")
new_dir <- file.path(tmp_root, "new")
files_daf(base_dir, name = "base", mode = "w+")
#> <dafr::FilesDaf>
#>  @ name                  : chr "base"
#>  @ internal              :<environment: 0x560b523a2108> 
#>  @ cache                 :<environment: 0x560b5239efc0> 
#>  @ axis_version_counter  :<environment: 0x560b523a0ae0> 
#>  @ vector_version_counter:<environment: 0x560b523a0db8> 
#>  @ matrix_version_counter:<environment: 0x560b5239d260> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x560b53f9c750> 
#>  @ cache                 :<environment: 0x560b53f9ca98> 
#>  @ axis_version_counter  :<environment: 0x560b53f6bd20> 
#>  @ vector_version_counter:<environment: 0x560b53f6bff8> 
#>  @ matrix_version_counter:<environment: 0x560b53f6c308> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x560b54315af8> 
#>  ..  ..@ cache                 :<environment: 0x560b542e9ad0> 
#>  ..  ..@ axis_version_counter  :<environment: 0x560b542dde50> 
#>  ..  ..@ vector_version_counter:<environment: 0x560b542dea20> 
#>  ..  ..@ matrix_version_counter:<environment: 0x560b542decf8> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x560b543a05b8> 
#>  ..  ..@ cache                 :<environment: 0x560b5439d470> 
#>  ..  ..@ axis_version_counter  :<environment: 0x560b5439b160> 
#>  ..  ..@ vector_version_counter:<environment: 0x560b5439b438> 
#>  ..  ..@ matrix_version_counter:<environment: 0x560b5439b710> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x560b543a05b8> 
#>  .. @ cache                 :<environment: 0x560b5439d470> 
#>  .. @ axis_version_counter  :<environment: 0x560b5439b160> 
#>  .. @ vector_version_counter:<environment: 0x560b5439b438> 
#>  .. @ matrix_version_counter:<environment: 0x560b5439b710> 
chain <- complete_daf(new_dir, "r")
```
