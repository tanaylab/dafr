# Add a new axis.

Add a new axis.

## Usage

``` r
add_axis(daf, axis, entries)
```

## Arguments

- daf:

  A `DafWriter`.

- axis:

  Axis name.

- entries:

  Unique, non-NA, non-empty character vector of entry names.

## Value

Invisibly the input `daf`.

## Examples

``` r
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2", "c3"))
axis_length(d, "cell")
#> [1] 3
```
