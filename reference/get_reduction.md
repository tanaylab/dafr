# Retrieve a registered reduction operation by name.

Looks up a reduction operation previously stored via
[`register_reduction()`](https://tanaylab.github.io/dafr/reference/register_reduction.md).
Raises if the name is not registered.

## Usage

``` r
get_reduction(name)
```

## Arguments

- name:

  Op name (character scalar).

## Value

The stored function.

## Examples

``` r
registered_reductions()   # "Sum" is among the built-ins
#>  [1] "Count"    "GeoMean"  "Max"      "Mean"     "Median"   "Min"     
#>  [7] "Mode"     "Quantile" "Std"      "StdN"     "Sum"      "Var"     
#> [13] "VarN"    
fn <- get_reduction("Sum")
fn(c(1, 2, 3))
#> [1] 6
```
