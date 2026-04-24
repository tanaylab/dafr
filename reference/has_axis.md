# Test whether an axis exists.

Test whether an axis exists.

## Usage

``` r
has_axis(daf, axis)
```

## Arguments

- daf:

  A `DafReader`.

- axis:

  Axis name (character scalar).

## Value

Logical scalar.

## Examples

``` r
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2"))
has_axis(d, "cell")
#> [1] TRUE
has_axis(d, "gene")
#> [1] FALSE
```
