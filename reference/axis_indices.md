# Look up 1-based positions of entries in an axis.

Look up 1-based positions of entries in an axis.

## Usage

``` r
axis_indices(daf, axis, entries, allow_empty = FALSE, allow_missing = FALSE)
```

## Arguments

- daf:

  A `DafReader`.

- axis:

  Axis name (character scalar).

- entries:

  Character vector of entry names to resolve.

- allow_empty:

  If `TRUE`, the empty string `""` resolves to a zero index (mirrors
  Julia `axis_indices(...; allow_empty=true)`).

- allow_missing:

  If `TRUE`, any non-empty name that is not present in the axis resolves
  to a zero index (mirrors Julia
  `axis_indices(...; allow_missing=true)`).

## Value

Integer vector of positions (1-based when present; `0L` when
`allow_empty`/`allow_missing` substitutes for an empty or absent name);
same length as `entries`.

## Examples

``` r
# Mirrors readers.jl jldoctest at line 389.
axis_indices(example_metacells_daf(), "type",
             c("MPP", ""), allow_empty = TRUE)
#> [1] 4 0
# 4 0
```
