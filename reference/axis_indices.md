# Get indices of entries in an axis

Returns the integer indices for specified entry names along an axis.

## Usage

``` r
axis_indices(daf, axis, entries, allow_empty = FALSE)
```

## Arguments

- daf:

  A Daf object

- axis:

  Name of the axis

- entries:

  Character vector of entry names to look up

- allow_empty:

  Whether to allow empty entries (return -1 for empty strings if TRUE)

## Value

A vector of 1-based indices corresponding to the entries

## Details

This function maps names to their position indices along the axis. If
`allow_empty` is TRUE, empty strings are converted to index -1. Indices
in R are 1-based (first element has index 1), consistent with R
conventions. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.axis_indices)
for details.
