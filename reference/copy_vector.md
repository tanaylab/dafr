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
  default = NULL,
  empty = NULL,
  overwrite = FALSE
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

## Value

The destination Daf object (invisibly)

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_vector!)
for details.
