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
#>  @ internal              :<environment: 0x55ae4699bd78> 
#>  @ cache                 :<environment: 0x55ae4699f080> 
#>  @ axis_version_counter  :<environment: 0x55ae4699d560> 
#>  @ vector_version_counter:<environment: 0x55ae4699d288> 
#>  @ matrix_version_counter:<environment: 0x55ae46910ea0> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55ae48899360> 
#>  @ cache                 :<environment: 0x55ae48899018> 
#>  @ axis_version_counter  :<environment: 0x55ae47fc96d8> 
#>  @ vector_version_counter:<environment: 0x55ae47fc9400> 
#>  @ matrix_version_counter:<environment: 0x55ae47fc9128> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x55ae4b183030> 
#>  ..  ..@ cache                 :<environment: 0x55ae4b3c9180> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55ae4ad9eb10> 
#>  ..  ..@ vector_version_counter:<environment: 0x55ae4ad9e838> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55ae4ad9e560> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x55ae4a666250> 
#>  ..  ..@ cache                 :<environment: 0x55ae4a669558> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55ae4abc59b8> 
#>  ..  ..@ vector_version_counter:<environment: 0x55ae4abc56e0> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55ae4abc5408> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x55ae4a666250> 
#>  .. @ cache                 :<environment: 0x55ae4a669558> 
#>  .. @ axis_version_counter  :<environment: 0x55ae4abc59b8> 
#>  .. @ vector_version_counter:<environment: 0x55ae4abc56e0> 
#>  .. @ matrix_version_counter:<environment: 0x55ae4abc5408> 
chain <- complete_daf(new_dir, "r")
```
