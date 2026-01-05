# Get dictionary of axis entries to indices

Returns a named vector that maps axis entry names to their corresponding
integer indices.

## Usage

``` r
axis_dict(daf, axis)
```

## Arguments

- daf:

  A Daf object

- axis:

  Name of the axis

## Value

A named vector mapping entry names to their 1-based indices

## Details

This function returns the mapping between entry names and their
positions along the axis. This is useful for efficient lookups when you
need to convert between names and indices repeatedly. In R, indices are
1-based (first element has index 1), consistent with R conventions. See
the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.axis_dict)
for details.
