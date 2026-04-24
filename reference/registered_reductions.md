# List registered reduction operations.

Returns the names of all currently registered reduction operations.

## Usage

``` r
registered_reductions()
```

## Value

Sorted character vector of registered reduction names.

## Examples

``` r
head(registered_reductions())
#> [1] "Count"          "GeoMean"        "Max"            "Mean"          
#> [5] "Median"         "Median_example"
register_reduction("Median_example", function(x, ...) median(x), overwrite = TRUE)
"Median_example" %in% registered_reductions()
#> [1] TRUE
```
