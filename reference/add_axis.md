# Add axis to a Daf object

Creates a new axis with the specified name and entries in the Daf data
set.

## Usage

``` r
add_axis(daf, axis, entries, overwrite = FALSE)
```

## Arguments

- daf:

  A Daf object

- axis:

  Name of the new axis

- entries:

  Vector of entry names (must be unique within the axis)

- overwrite:

  Whether to overwrite if axis already exists (FALSE by default)

## Value

The Daf object (invisibly, for chaining operations)

## Details

This function creates a new axis with the specified unique entry names.
If the axis already exists and `overwrite` is FALSE, an error will be
raised. Entry names must be unique within the axis. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.add_axis!)
for details.
