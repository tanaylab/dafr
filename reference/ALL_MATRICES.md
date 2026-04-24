# Wildcard for all matrices.

Wildcard for all matrices.

## Usage

``` r
ALL_MATRICES
```

## Format

An object of class `character` of length 3.

## Examples

``` r
d <- example_cells_daf()
v <- viewer(d, data = list(list(ALL_MATRICES, "=")))
matrices_set(v, "cell", "gene")
#> [1] "UMIs"
```
