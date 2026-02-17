# Copy a scalar from source to destination

Copy a scalar from source to destination

## Usage

``` r
copy_scalar(
  destination,
  source,
  name,
  rename = NULL,
  default,
  overwrite = FALSE,
  type = NULL,
  insist = TRUE
)
```

## Arguments

- destination:

  A Daf object to copy to

- source:

  A Daf object to copy from

- name:

  Name of the scalar to copy

- rename:

  Optional new name for the scalar in the destination

- default:

  Default value if scalar doesn't exist. If not provided (the default),
  an error will be raised if the scalar is missing in the source. If
  explicitly set to NULL, the copy will silently skip missing scalars.

- overwrite:

  Whether to overwrite if scalar already exists

- type:

  Optional type to convert the scalar to (e.g., "Float64", "Int32"). If
  NULL, the original type is preserved.

- insist:

  Whether to skip if the destination already has the scalar (FALSE) or
  to attempt the copy regardless (TRUE, the default). Only relevant when
  overwrite=FALSE.

## Value

The destination Daf object (invisibly)

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_scalar!)
for details.
