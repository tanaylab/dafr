# Look up 1-based positions of entries in an axis.

Look up 1-based positions of entries in an axis.

## Usage

``` r
axis_indices(daf, axis, entries)
```

## Arguments

- daf:

  A `DafReader`.

- axis:

  Axis name (character scalar).

- entries:

  Character vector of entry names to resolve.

## Value

Integer vector of 1-based positions; same length as `entries`.

## Examples

``` r
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2", "c3"))
axis_indices(d, "cell", c("c3", "c1"))
#> [1] 3 1
```
