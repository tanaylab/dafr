# Get axis version counter

Returns the version counter for an axis, which is incremented when the
axis is modified. This is useful for cache invalidation.

## Usage

``` r
axis_version_counter(daf, axis)
```

## Arguments

- daf:

  A Daf object

- axis:

  Name of the axis

## Value

An integer version counter

## Details

The version counter is incremented whenever the axis entries are
modified. This can be used to detect changes and invalidate caches.
