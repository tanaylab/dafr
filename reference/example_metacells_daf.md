# Load metacells example data into a MemoryDaf.

Replicates the Julia `ExampleData.example_metacells_daf()` dataset.

## Usage

``` r
example_metacells_daf(name = "metacells!")
```

## Arguments

- name:

  Name for the returned `MemoryDaf`.

## Value

A [`MemoryDaf`](https://tanaylab.github.io/dafr/reference/MemoryDaf.md)
pre-populated with 856 cells, 683 genes, 7 metacells, and 4 types.

## Examples

``` r
# Mirrors example_data.jl jldoctest at line 62.
m <- example_metacells_daf()
cat(description(m))
#> name: metacells!
#> type: MemoryDaf
#> axes:
#>   cell: 856 entries
#>   gene: 683 entries
#>   metacell: 7 entries
#>   type: 4 entries
#> vectors:
#>   cell:
#>     metacell
#>   gene:
#>     is_marker
#>   metacell:
#>     type
#>   type:
#>     color
#> matrices:
#>   gene,metacell:
#>     fraction
#>   metacell,metacell:
#>     edge_weight
```
