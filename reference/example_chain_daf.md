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
ch <- example_chain_daf()
axes_set(ch)
#> [1] "cell"       "donor"      "experiment" "gene"       "metacell"  
#> [6] "type"      
```
