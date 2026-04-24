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
m <- example_metacells_daf()
axes_set(m)
#> [1] "cell"     "gene"     "metacell" "type"    
```
