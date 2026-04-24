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
d <- example_cells_daf()
axes_set(d)
#> [1] "cell"       "donor"      "experiment" "gene"      
axis_length(d, "cell")
#> [1] 856
```
