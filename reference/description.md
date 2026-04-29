# Human-readable summary of a Daf store.

Returns a multi-line string describing axes, scalars, vectors, and
matrices. Matches the column-order rendering of Julia DAF's own
`description()`.

## Usage

``` r
description(daf)
```

## Arguments

- daf:

  A `DafReader`.

## Value

Character scalar.

## Examples

``` r
# Mirrors readers.jl jldoctest at line 1170 (description of the chain).
cat(description(example_chain_daf()))
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
