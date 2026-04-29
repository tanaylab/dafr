# Set a matrix indexed by a pair of axes.

Set a matrix indexed by a pair of axes.

## Usage

``` r
set_matrix(
  daf,
  rows_axis,
  columns_axis,
  name,
  mat,
  overwrite = FALSE,
  relayout = FALSE
)
```

## Arguments

- daf:

  A `DafReader`.

- rows_axis:

  Row-axis name.

- columns_axis:

  Column-axis name.

- name:

  Matrix name.

- mat:

  Dense `matrix`, or sparse `dgCMatrix` / `lgCMatrix`, of shape
  `axis_length(rows_axis) x axis_length(columns_axis)`.

- overwrite:

  See `set_scalar`.

- relayout:

  If `TRUE`, also store the flipped layout (so
  `get_matrix(columns_axis, rows_axis, name)` skips the
  transpose-on-the-fly path). Mirrors Julia
  `set_matrix!(...; relayout)`. Default `FALSE`; set to `TRUE` to match
  Julia's default behavior.

## Value

Invisibly the input `daf`.

## Examples

``` r
# Mirrors writers.jl jldoctest at line 686.
m <- example_metacells_daf()
has_matrix(m, "gene", "metacell", "confidence", relayout = FALSE)    # FALSE
#> [1] FALSE
set_matrix(m, "metacell", "gene", "confidence",
           matrix(stats::runif(7L * 683L), 7L, 683L), relayout = FALSE)
has_matrix(m, "gene", "metacell", "confidence", relayout = FALSE)    # FALSE
#> [1] FALSE
has_matrix(m, "metacell", "gene", "confidence", relayout = FALSE)    # TRUE
#> [1] TRUE
set_matrix(m, "metacell", "gene", "confidence",
           matrix(stats::runif(7L * 683L), 7L, 683L),
           overwrite = TRUE, relayout = TRUE)
has_matrix(m, "gene", "metacell", "confidence", relayout = FALSE)    # TRUE
#> [1] TRUE
has_matrix(m, "metacell", "gene", "confidence", relayout = FALSE)    # TRUE
#> [1] TRUE
```
