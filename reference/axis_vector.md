# Get vector of axis entries from a Daf object

Returns a vector of the unique names for all entries of the specified
axis.

## Usage

``` r
axis_vector(daf, axis, null_if_missing = FALSE)
```

## Arguments

- daf:

  A Daf object

- axis:

  Name of the axis

- null_if_missing:

  Whether to return NULL if the axis doesn't exist

## Value

A character vector of axis entry names

## Details

Axis entries provide names for each position along an axis, such as gene
names for a "gene" axis or cell barcodes for a "cell" axis. These entry
names can be used to look up specific indices using the
[`axis_indices()`](https://tanaylab.github.io/dafr/reference/axis_indices.md)
function. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.axis_vector)
for details.
