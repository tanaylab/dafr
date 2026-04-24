# Construct a ViewDaf over a base daf.

Construct a ViewDaf over a base daf.

## Usage

``` r
viewer(daf, name = NULL, axes = NULL, data = NULL)
```

## Arguments

- daf:

  Base `DafReader`.

- name:

  Name for the view (defaults to `<daf-name>.view`).

- axes:

  Optional list of axis overrides. V1 path: `NULL` means expose every
  base axis as-is.

- data:

  Optional list of data overrides. V1 path: unused; the no-override
  behaviour is always "expose everything as-is".

## Value

A `ViewDaf`.

## Examples

``` r
d <- example_cells_daf()
# NULL query drops an axis from the view:
v <- viewer(d, axes = list(list(ALL_AXES, "="), list("gene", NULL)))
axes_set(v)
#> [1] "cell"       "donor"      "experiment"
vectors_set(v, "cell")
#> [1] "donor"      "experiment"
```
