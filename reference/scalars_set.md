# Names of all scalars, sorted.

Names of all scalars, sorted.

## Usage

``` r
scalars_set(daf)
```

## Arguments

- daf:

  A `DafReader`.

## Value

Character vector.

## Examples

``` r
# Mirrors readers.jl jldoctest at line 125.
scalars_set(example_cells_daf()) # "organism" "reference"
#> [1] "organism"  "reference"
```
