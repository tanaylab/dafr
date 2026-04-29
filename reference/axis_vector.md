# Entry-name vector for an axis.

Entry-name vector for an axis.

## Usage

``` r
axis_vector(daf, axis, null_if_missing = FALSE)
```

## Arguments

- daf:

  A `DafReader`.

- axis:

  Axis name (character scalar).

- null_if_missing:

  If `TRUE`, return `NULL` when the axis is absent instead of raising.

## Value

Character vector of entry names.

## Examples

``` r
# Mirrors readers.jl jldoctest at line 308.
axis_vector(example_metacells_daf(), "type")
#> [1] "memory-B" "MEBEMP-E" "MEBEMP-L" "MPP"     
# "memory-B" "MEBEMP-E" "MEBEMP-L" "MPP"
axis_vector(example_cells_daf(), "missing", null_if_missing = TRUE)
#> NULL
```
