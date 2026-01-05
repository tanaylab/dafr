# Get length of an axis in a Daf object

Returns the number of entries along the specified axis in the Daf data
set.

## Usage

``` r
axis_length(daf, axis)
```

## Arguments

- daf:

  A Daf object

- axis:

  Name of the axis

## Value

Length (number of entries) of the axis

## Details

The axis length corresponds to the size of vector properties for this
axis and to one of the dimensions of matrix properties involving this
axis. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.axis_length)
for details.
