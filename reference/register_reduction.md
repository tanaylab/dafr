# Register a reduction operation.

Stores a user-defined reduction function under the given name so that it
can be retrieved by
[`get_reduction()`](https://tanaylab.github.io/dafr/reference/get_reduction.md)
and invoked during query evaluation.

## Usage

``` r
register_reduction(name, fn, overwrite = FALSE)
```

## Arguments

- name:

  Op name (character scalar, matches token in query strings).

- fn:

  Function `function(x, ...)` where `x` is a numeric vector or matrix
  column and `...` collects named parameters.

- overwrite:

  Logical scalar; set to `TRUE` to replace an already- registered
  operation.

## Value

Invisibly `NULL`.

## Examples

``` r
register_reduction("Median_example", function(x, ...) median(x, ...), overwrite = TRUE)
registered_reductions()
#>  [1] "Count"          "GeoMean"        "Max"            "Mean"          
#>  [5] "Median"         "Median_example" "Min"            "Mode"          
#>  [9] "Quantile"       "Std"            "StdN"           "Sum"           
#> [13] "Var"            "VarN"          
```
