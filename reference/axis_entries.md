# Entry names of an axis (full or by index).

Entry names of an axis (full or by index).

## Usage

``` r
axis_entries(daf, axis, indices = NULL, allow_empty = FALSE)
```

## Arguments

- daf:

  A `DafReader`.

- axis:

  Axis name (character scalar).

- indices:

  Optional integer index vector (1-based). When `allow_empty = TRUE`, a
  zero or negative index is allowed and is translated to the empty
  string `""` in the result.

- allow_empty:

  If `TRUE`, treat zero/negative `indices` as the empty string `""` in
  the result (mirrors Julia `axis_entries(...; allow_empty=true)`).

## Value

Character vector.

## Examples

``` r
# Mirrors readers.jl jldoctest at line 448.
axis_entries(example_metacells_daf(), "type",
             indices = c(3L, 0L), allow_empty = TRUE)
#> [1] "MEBEMP-L" ""        
# "MEBEMP-L" ""
```
