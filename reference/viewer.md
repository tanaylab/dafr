# Create a read-only view of a Daf data set

Wrap a Daf data set with a read-only DafView

## Usage

``` r
viewer(daf, name = NULL, axes = NULL, data = NULL)
```

## Arguments

- daf:

  A Daf object

- name:

  Optional name for the view

- axes:

  Named list specifying axes to expose

- data:

  Named list specifying data to expose

## Value

A read-only Daf object

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/views.html#DataAxesFormats.Views.viewer)
for details.
