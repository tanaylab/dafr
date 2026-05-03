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
#>  @ internal              :<environment: 0x559dffe74e10> 
#>  @ cache                 :<environment: 0x559dffe742e8> 
#>  @ axis_version_counter  :<environment: 0x559dffe765f8> 
#>  @ vector_version_counter:<environment: 0x559dffe76320> 
#>  @ matrix_version_counter:<environment: 0x559dffe76048> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x559e022c9898> 
#>  @ cache                 :<environment: 0x559e022c9550> 
#>  @ axis_version_counter  :<environment: 0x559e02247f50> 
#>  @ vector_version_counter:<environment: 0x559e02247c78> 
#>  @ matrix_version_counter:<environment: 0x559e022479a0> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x559e035829a0> 
#>  ..  ..@ cache                 :<environment: 0x559dffccc280> 
#>  ..  ..@ axis_version_counter  :<environment: 0x559dfc4908f0> 
#>  ..  ..@ vector_version_counter:<environment: 0x559dfc490618> 
#>  ..  ..@ matrix_version_counter:<environment: 0x559dfc490340> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x559e000f1920> 
#>  ..  ..@ cache                 :<environment: 0x559e000f0df8> 
#>  ..  ..@ axis_version_counter  :<environment: 0x559e000f3108> 
#>  ..  ..@ vector_version_counter:<environment: 0x559e000f2e30> 
#>  ..  ..@ matrix_version_counter:<environment: 0x559dffb0c998> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x559e000f1920> 
#>  .. @ cache                 :<environment: 0x559e000f0df8> 
#>  .. @ axis_version_counter  :<environment: 0x559e000f3108> 
#>  .. @ vector_version_counter:<environment: 0x559e000f2e30> 
#>  .. @ matrix_version_counter:<environment: 0x559dffb0c998> 
chain <- complete_daf(new_dir, "r")
```
