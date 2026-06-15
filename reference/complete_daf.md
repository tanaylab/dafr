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
#>  @ internal              :<environment: 0x559023387d40> 
#>  @ cache                 :<environment: 0x559023387218> 
#>  @ axis_version_counter  :<environment: 0x559023389528> 
#>  @ vector_version_counter:<environment: 0x559023389250> 
#>  @ matrix_version_counter:<environment: 0x559023388f78> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x55902a567f48> 
#>  @ cache                 :<environment: 0x55902a567c00> 
#>  @ axis_version_counter  :<environment: 0x55902a569f10> 
#>  @ vector_version_counter:<environment: 0x55902a569c38> 
#>  @ matrix_version_counter:<environment: 0x55902a569960> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x559025760c80> 
#>  ..  ..@ cache                 :<environment: 0x55902455c120> 
#>  ..  ..@ axis_version_counter  :<environment: 0x55902455e430> 
#>  ..  ..@ vector_version_counter:<environment: 0x55902455e158> 
#>  ..  ..@ matrix_version_counter:<environment: 0x55902455de80> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x559023c26430> 
#>  ..  ..@ cache                 :<environment: 0x559023c25908> 
#>  ..  ..@ axis_version_counter  :<environment: 0x559023c27c18> 
#>  ..  ..@ vector_version_counter:<environment: 0x559023c27940> 
#>  ..  ..@ matrix_version_counter:<environment: 0x559023c27668> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x559023c26430> 
#>  .. @ cache                 :<environment: 0x559023c25908> 
#>  .. @ axis_version_counter  :<environment: 0x559023c27c18> 
#>  .. @ vector_version_counter:<environment: 0x559023c27940> 
#>  .. @ matrix_version_counter:<environment: 0x559023c27668> 
chain <- complete_daf(new_dir, "r")
```
