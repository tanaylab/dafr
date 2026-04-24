# List registered eltwise operations.

Returns the names of all currently registered eltwise operations.

## Usage

``` r
registered_eltwise()
```

## Value

Sorted character vector of registered eltwise names.

## Examples

``` r
head(registered_eltwise())
#> [1] "Abs"      "Clamp"    "Clamp01"  "Convert"  "Exp"      "Fraction"
register_eltwise("Negate_example", function(x, ...) -x, overwrite = TRUE)
"Negate_example" %in% registered_eltwise()
#> [1] TRUE
```
