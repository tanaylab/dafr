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
#>  @ internal              :<environment: 0x56349513a358> 
#>  @ cache                 :<environment: 0x5634950d6460> 
#>  @ axis_version_counter  :<environment: 0x5634950d8770> 
#>  @ vector_version_counter:<environment: 0x5634950d8498> 
#>  @ matrix_version_counter:<environment: 0x5634950d81c0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x56349b8d9d80> 
#>  @ cache                 :<environment: 0x56349b8d9a38> 
#>  @ axis_version_counter  :<environment: 0x56349b8dbd48> 
#>  @ vector_version_counter:<environment: 0x56349b8dba70> 
#>  @ matrix_version_counter:<environment: 0x56349b8db798> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x56349a042348> 
#>  ..  ..@ cache                 :<environment: 0x56349a4a2e08> 
#>  ..  ..@ axis_version_counter  :<environment: 0x56349a4a5118> 
#>  ..  ..@ vector_version_counter:<environment: 0x56349a4a4e40> 
#>  ..  ..@ matrix_version_counter:<environment: 0x56349a4a4b68> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x56349c06ea38> 
#>  ..  ..@ cache                 :<environment: 0x56349c06df10> 
#>  ..  ..@ axis_version_counter  :<environment: 0x56349c070220> 
#>  ..  ..@ vector_version_counter:<environment: 0x56349c06ff48> 
#>  ..  ..@ matrix_version_counter:<environment: 0x56349c06fc70> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x56349c06ea38> 
#>  .. @ cache                 :<environment: 0x56349c06df10> 
#>  .. @ axis_version_counter  :<environment: 0x56349c070220> 
#>  .. @ vector_version_counter:<environment: 0x56349c06ff48> 
#>  .. @ matrix_version_counter:<environment: 0x56349c06fc70> 
chain <- complete_daf(new_dir, "r")
```
