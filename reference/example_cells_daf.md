# Load cells example data into a MemoryDaf.

Replicates the Julia `ExampleData.example_cells_daf()` dataset. Data are
loaded from the bundled `inst/extdata/example_data/` files and
type-promoted using the same lattice as Julia (Bool → UInt32/Int32 →
Float32 for vectors; UInt8 → UInt16 → Float32 for matrices).

## Usage

``` r
example_cells_daf(name = "cells!")
```

## Arguments

- name:

  Name for the returned `MemoryDaf`.

## Value

A [`MemoryDaf`](https://tanaylab.github.io/dafr/reference/MemoryDaf.md)
pre-populated with 856 cells, 683 genes, 95 donors, and 23 experiments.

## Examples

``` r
# Mirrors example_data.jl jldoctest at line 22.
d <- example_cells_daf()
cat(description(d))
#> name: cells!
#> type: MemoryDaf
#> scalars:
#>   organism: "human"
#>   reference: "test"
#> axes:
#>   cell: 856 entries
#>   donor: 95 entries
#>   experiment: 23 entries
#>   gene: 683 entries
#> vectors:
#>   cell:
#>     donor
#>     experiment
#>   donor:
#>     age
#>     sex
#>   gene:
#>     is_lateral
#> matrices:
#>   cell,gene:
#>     UMIs
#>   gene,cell:
#>     UMIs
```
