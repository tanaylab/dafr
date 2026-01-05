# Generate names for groups of axis entries

Given groups of axis entries (as indices), generates unique names for
each group based on the entry names using a hash.

## Usage

``` r
group_names(daf, axis, entries_of_groups, prefix)
```

## Arguments

- daf:

  A Daf object

- axis:

  Name of the axis

- entries_of_groups:

  A list of integer vectors, where each vector contains the indices of
  entries belonging to a group

- prefix:

  A prefix to add to each generated name

## Value

A character vector of unique group names

## Details

This function generates deterministic names for groups based on their
member entries. Groups with the same members will always get the same
name. This is useful for creating reproducible group identifiers.
