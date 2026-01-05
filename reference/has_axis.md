# Check if an axis exists in a Daf object

Determines whether an axis with the specified name exists in the Daf
data set.

## Usage

``` r
has_axis(daf, axis)
```

## Arguments

- daf:

  A Daf object

- axis:

  Name of the axis to check

## Value

TRUE if the axis exists, FALSE otherwise

## Details

Axes are fundamental dimensions in a Daf data set along which vector and
matrix data are stored. Each axis has a collection of unique named
entries. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.has_axis)
for details.
