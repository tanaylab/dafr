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
# Mirrors readers.jl jldoctest at line 353.
dict <- axis_dict(example_metacells_daf(), "type")
dict[["memory-B"]] # 1
#> [1] 1
dict[["MPP"]]      # 4
#> [1] 4
```
