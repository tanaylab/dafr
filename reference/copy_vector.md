# Copy a vector from source to destination

Copy a vector from source to destination

## Usage

``` r
copy_vector(
  destination,
  source,
  axis,
  name,
  reaxis = NULL,
  rename = NULL,
  default,
  empty = NULL,
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

- axis:

  Name of the axis

- name:

  Name of the vector to copy

- reaxis:

  Optional different axis name in the destination

- rename:

  Optional new name for the vector in the destination

- default:

  Default value if vector doesn't exist

- empty:

  Value to use for filling in missing data

- overwrite:

  Whether to overwrite if vector already exists

- eltype:

  Optional element type to convert to (e.g., "Float64", "Int32"). If
  NULL, the original type is preserved.

- bestify:

  Whether to bestify the vector storage (FALSE by default)

- min_sparse_saving_fraction:

  Optional minimum sparse saving fraction. If NULL, the default is used.

- insist:

  Whether to fail if the vector doesn't exist (TRUE by default)

## Value

The destination Daf object (invisibly)

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_vector!)
for details.
