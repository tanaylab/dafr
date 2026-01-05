# Get matrix version counter

Returns the version counter for a matrix property, which is incremented
when the matrix is modified. This is useful for cache invalidation.

## Usage

``` r
matrix_version_counter(daf, rows_axis, columns_axis, name)
```

## Arguments

- daf:

  A Daf object

- rows_axis:

  Name of the rows axis

- columns_axis:

  Name of the columns axis

- name:

  Name of the matrix property

## Value

An integer version counter

## Details

The version counter is incremented whenever the matrix data is modified.
This can be used to detect changes and invalidate caches.
