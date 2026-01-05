# Get vector version counter

Returns the version counter for a vector property, which is incremented
when the vector is modified. This is useful for cache invalidation.

## Usage

``` r
vector_version_counter(daf, axis, name)
```

## Arguments

- daf:

  A Daf object

- axis:

  Name of the axis

- name:

  Name of the vector property

## Value

An integer version counter

## Details

The version counter is incremented whenever the vector data is modified.
This can be used to detect changes and invalidate caches.
