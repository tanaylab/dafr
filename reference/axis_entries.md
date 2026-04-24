# Entry names of an axis (full or by index).

Entry names of an axis (full or by index).

## Usage

``` r
axis_entries(daf, axis, indices = NULL)
```

## Arguments

- daf:

  A `DafReader`.

- axis:

  Axis name (character scalar).

- indices:

  Optional integer index vector (1-based).

## Value

Character vector.

## Examples

``` r
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
axis_entries(d, "cell")
#> [1] "c1" "c2" "c3" "c4"
axis_entries(d, "cell", indices = c(1L, 3L))
#> [1] "c1" "c3"
```
