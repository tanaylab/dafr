# Get entry names for indices in an axis

Returns the entry names for specified indices along an axis.

## Usage

``` r
axis_entries(daf, axis, indices = NULL, allow_empty = FALSE)
```

## Arguments

- daf:

  A Daf object

- axis:

  Name of the axis

- indices:

  Vector of 1-based integer indices (or NULL for all entries)

- allow_empty:

  Whether to allow empty/invalid indices (return empty strings if TRUE)

## Value

A character vector of entry names corresponding to the indices

## Details

This function maps position indices to their names along the axis. If
`indices` is NULL, returns all entries of the axis. If `allow_empty` is
TRUE and an invalid index is provided, an empty string is returned for
that position. Indices must be positive integers and within the bounds
of the axis length. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.axis_entries)
for details.
