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
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2", "c3"))
axis_vector(d, "cell")
#> [1] "c1" "c2" "c3"
axis_vector(d, "gene", null_if_missing = TRUE)
#> NULL
```
