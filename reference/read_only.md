# Create a read-only wrapper for a Daf object

Creates a read-only view of a Daf object to protect it against
accidental modification.

## Usage

``` r
read_only(daf, name = NULL)
```

## Arguments

- daf:

  A Daf object

- name:

  Optional name for the read-only wrapper (defaults to the original
  name)

## Value

A read-only Daf object

## Details

This function wraps a Daf object with a read-only interface to protect
against accidental modification. Any attempt to modify the data will
result in an error. The read-only wrapper can be efficiently created as
it shares data with the original object. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/read_only.html#DataAxesFormats.ReadOnly.read_only)
for details.
