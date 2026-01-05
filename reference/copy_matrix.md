# Copy a matrix from source to destination

Copy a matrix from source to destination

## Usage

``` r
copy_matrix(
  destination,
  source,
  rows_axis,
  columns_axis,
  name,
  rows_reaxis = NULL,
  columns_reaxis = NULL,
  rename = NULL,
  default = NULL,
  empty = NULL,
  relayout = TRUE,
  overwrite = FALSE
)
```

## Arguments

- destination:

  A Daf object to copy to

- source:

  A Daf object to copy from

- rows_axis:

  Name of rows axis

- columns_axis:

  Name of columns axis

- name:

  Name of the matrix to copy

- rows_reaxis:

  Optional different rows axis name in the destination

- columns_reaxis:

  Optional different columns axis name in the destination

- rename:

  Optional new name for the matrix in the destination

- default:

  Default value if matrix doesn't exist

- empty:

  Value to use for filling in missing data

- relayout:

  Whether to allow relayout

- overwrite:

  Whether to overwrite if matrix already exists

## Value

The destination Daf object (invisibly)

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_matrix!)
for details.
