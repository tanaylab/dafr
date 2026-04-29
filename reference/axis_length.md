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
# Mirrors readers.jl jldoctest at line 487.
axis_length(example_metacells_daf(), "type") # 4
#> [1] 4
```
