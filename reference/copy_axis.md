# Copy an axis from source to destination

Copy an axis from source to destination

## Usage

``` r
copy_axis(
  destination,
  source,
  axis,
  rename = NULL,
  default = NULL,
  overwrite = FALSE,
  insist = TRUE
)
```

## Arguments

- destination:

  A Daf object to copy to

- source:

  A Daf object to copy from

- axis:

  Name of the axis to copy

- rename:

  Optional new name for the axis in the destination

- default:

  Default value if axis doesn't exist

- overwrite:

  Whether to overwrite if axis already exists

- insist:

  Whether to fail if the axis doesn't exist

## Value

The destination Daf object (invisibly)

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_axis!)
for details.
