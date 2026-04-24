# Entry-name to 1-based-index hash for an axis.

Entry-name to 1-based-index hash for an axis.

## Usage

``` r
axis_dict(daf, axis)
```

## Arguments

- daf:

  A `DafReader`.

- axis:

  Axis name (character scalar).

## Value

An environment mapping entry names to integer positions.

## Examples

``` r
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2", "c3"))
dict <- axis_dict(d, "cell")
dict[["c2"]]
#> [1] 2
```
