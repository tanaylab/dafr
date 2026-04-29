# Names of all axes, sorted.

Names of all axes, sorted.

## Usage

``` r
axes_set(daf)
```

## Arguments

- daf:

  A `DafReader`.

## Value

Character vector of axis names.

## Examples

``` r
# Mirrors readers.jl jldoctest at line 273.
axes_set(example_cells_daf()) # "cell" "donor" "experiment" "gene"
#> [1] "cell"       "donor"      "experiment" "gene"      
```
