# Get a vector, returning it as an axis-named R vector.

Get a vector, returning it as an axis-named R vector.

## Usage

``` r
get_vector(daf, axis, name, default)
```

## Arguments

- daf:

  A `DafReader`.

- axis:

  Axis name.

- name:

  Vector name.

- default:

  If supplied and the vector is absent, return a named vector of length
  `axis_length(daf, axis)` with the axis entries as names. A length-1
  `default` is recycled to every entry; a length-N `default` (matching
  the axis length) is used as-is. Any other length is an error. The
  vector's atomic type follows `default` (e.g. `default = NA` yields
  `logical`, `default = "x"` yields `character`, `default = 0.0` yields
  `double`).

## Value

Named atomic vector.

## Examples

``` r
d <- example_cells_daf()
donor <- get_vector(d, "cell", "donor")
head(donor)
#> demux_07_12_20_1_AACAAGATCCATTTCA-1 demux_07_12_20_1_AACGAAAGTCCAATCA-1 
#>                               "N89"                               "N84" 
#> demux_07_12_20_1_AAGACAAAGTTCCGTA-1 demux_07_12_20_1_AGACTCATCTATTGTC-1 
#>                               "N86"                               "N84" 
#> demux_07_12_20_1_AGATAGACATTCCTCG-1 demux_07_12_20_1_ATCGTAGTCCAGTGCG-1 
#>                               "N89"                               "N89" 
# default value for a missing vector (recycled to axis length):
head(get_vector(d, "cell", "missing_vec", default = NA_character_))
#> demux_07_12_20_1_AACAAGATCCATTTCA-1 demux_07_12_20_1_AACGAAAGTCCAATCA-1 
#>                                  NA                                  NA 
#> demux_07_12_20_1_AAGACAAAGTTCCGTA-1 demux_07_12_20_1_AGACTCATCTATTGTC-1 
#>                                  NA                                  NA 
#> demux_07_12_20_1_AGATAGACATTCCTCG-1 demux_07_12_20_1_ATCGTAGTCCAGTGCG-1 
#>                                  NA                                  NA 
```
