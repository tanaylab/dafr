# Copy a tensor from source to destination

Copy a tensor from source to destination

## Usage

``` r
copy_tensor(
  destination,
  source,
  main_axis,
  rows_axis,
  columns_axis,
  name,
  rows_reaxis = NULL,
  columns_reaxis = NULL,
  rename = NULL,
  empty = NULL,
  relayout = TRUE,
  overwrite = FALSE,
  eltype = NULL,
  bestify = FALSE,
  min_sparse_saving_fraction = NULL,
  insist = TRUE
)
```

## Arguments

- destination:

  A Daf object to copy to

- source:

  A Daf object to copy from

- main_axis:

  Name of main axis

- rows_axis:

  Name of rows axis

- columns_axis:

  Name of columns axis

- name:

  Name of the tensor

- rows_reaxis:

  Optional different rows axis name in the destination

- columns_reaxis:

  Optional different columns axis name in the destination

- rename:

  Optional new name for the tensor in the destination

- empty:

  Value to use for filling in missing data

- relayout:

  Whether to allow relayout

- overwrite:

  Whether to overwrite if tensor already exists

- eltype:

  Optional element type to convert to (e.g., "Float64", "Int32"). If
  NULL, the original type is preserved.

- bestify:

  Whether to bestify the tensor storage (FALSE by default)

- min_sparse_saving_fraction:

  Optional minimum sparse saving fraction. If NULL, the default is used.

- insist:

  Whether to fail if the tensor doesn't exist (TRUE by default)

## Value

The destination Daf object (invisibly)

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_tensor!)
for details.
