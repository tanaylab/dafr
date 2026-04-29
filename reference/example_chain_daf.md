# Load a chain of cells and metacells example data.

Replicates the Julia `ExampleData.example_chain_daf()` dataset.

## Usage

``` r
example_chain_daf(name = "chain!")
```

## Arguments

- name:

  Name for the returned `WriteChainDaf`.

## Value

A
[`WriteChainDaf`](https://tanaylab.github.io/dafr/reference/WriteChainDaf.md)
combining
[`example_cells_daf()`](https://tanaylab.github.io/dafr/reference/example_cells_daf.md)
and
[`example_metacells_daf()`](https://tanaylab.github.io/dafr/reference/example_metacells_daf.md).

## Examples

``` r
# Mirrors example_data.jl jldoctest at line 205.
ch <- example_chain_daf()
cat(description(ch))
#> name: chain!
#> type: WriteChainDaf
#> scalars:
#>   organism: "human"
#>   reference: "test"
#> axes:
#>   cell: 856 entries
#>   donor: 95 entries
#>   experiment: 23 entries
#>   gene: 683 entries
#>   metacell: 7 entries
#>   type: 4 entries
#> vectors:
#>   cell:
#>     donor
#>     experiment
#>     metacell
#>   donor:
#>     age
#>     sex
#>   gene:
#>     is_lateral
#>     is_marker
#>   metacell:
#>     type
#>   type:
#>     color
#> matrices:
#>   cell,gene:
#>     UMIs
#>   gene,cell:
#>     UMIs
#>   gene,metacell:
#>     fraction
#>   metacell,metacell:
#>     edge_weight
```
