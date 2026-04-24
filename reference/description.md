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
d <- memory_daf(name = "demo")
set_scalar(d, "organism", "human")
add_axis(d, "cell", c("c1", "c2"))
set_vector(d, "cell", "donor", c("d1", "d2"))
cat(description(d))
#> name: demo
#> type: MemoryDaf
#> scalars:
#>   organism: "human"
#> axes:
#>   cell: 2 entries
#> vectors:
#>   cell:
#>     donor
```
