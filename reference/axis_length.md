# Length (entry count) of an axis.

Length (entry count) of an axis.

## Usage

``` r
axis_length(daf, axis)
```

## Arguments

- daf:

  A `DafReader`.

- axis:

  Axis name (character scalar).

## Value

Integer scalar.

## Examples

``` r
d <- example_cells_daf()
axis_length(d, "cell")
#> [1] 856
```
