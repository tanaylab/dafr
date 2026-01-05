# Copy a scalar from source to destination

Copy a scalar from source to destination

## Usage

``` r
copy_scalar(
  destination,
  source,
  name,
  rename = NULL,
  default = NULL,
  overwrite = FALSE
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

  Default value if scalar doesn't exist

- overwrite:

  Whether to overwrite if scalar already exists

## Value

The destination Daf object (invisibly)

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_scalar!)
for details.
