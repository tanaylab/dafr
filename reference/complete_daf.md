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
#>  @ internal              :<environment: 0x561065add440> 
#>  @ cache                 :<environment: 0x561065adc918> 
#>  @ axis_version_counter  :<environment: 0x561065adec28> 
#>  @ vector_version_counter:<environment: 0x561065ade950> 
#>  @ matrix_version_counter:<environment: 0x561065ae24a8> 
new <- files_daf(new_dir, name = "new", mode = "w+")
complete_chain(
    base_daf = open_daf(base_dir, "r"),
    new_daf = new, absolute = TRUE
)
#> <dafr::WriteChainDaf>
#>  @ name                  : chr "new"
#>  @ internal              :<environment: 0x56106c105bf0> 
#>  @ cache                 :<environment: 0x56106c1058a8> 
#>  @ axis_version_counter  :<environment: 0x561069d69388> 
#>  @ vector_version_counter:<environment: 0x561069d690b0> 
#>  @ matrix_version_counter:<environment: 0x561069d68dd8> 
#>  @ dafs                  :List of 2
#>  .. $ : <dafr::FilesDafReadOnly>
#>  ..  ..@ name                  : chr "base"
#>  ..  ..@ internal              :<environment: 0x561067d60418> 
#>  ..  ..@ cache                 :<environment: 0x5610669ae118> 
#>  ..  ..@ axis_version_counter  :<environment: 0x5610669b0428> 
#>  ..  ..@ vector_version_counter:<environment: 0x5610669b0150> 
#>  ..  ..@ matrix_version_counter:<environment: 0x5610669afe78> 
#>  .. $ : <dafr::FilesDaf>
#>  ..  ..@ name                  : chr "new"
#>  ..  ..@ internal              :<environment: 0x56106c363c60> 
#>  ..  ..@ cache                 :<environment: 0x56106c363138> 
#>  ..  ..@ axis_version_counter  :<environment: 0x56106c365448> 
#>  ..  ..@ vector_version_counter:<environment: 0x56106c365170> 
#>  ..  ..@ matrix_version_counter:<environment: 0x561066616388> 
#>  @ writer                : <dafr::FilesDaf>
#>  .. @ name                  : chr "new"
#>  .. @ internal              :<environment: 0x56106c363c60> 
#>  .. @ cache                 :<environment: 0x56106c363138> 
#>  .. @ axis_version_counter  :<environment: 0x56106c365448> 
#>  .. @ vector_version_counter:<environment: 0x56106c365170> 
#>  .. @ matrix_version_counter:<environment: 0x561066616388> 
chain <- complete_daf(new_dir, "r")
```
