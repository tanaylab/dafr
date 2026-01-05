# Compact group indices

Given a vector of group indices (where 0 means no group), compacts the
indices so they are in the range 1 to N, where N is the number of unique
groups.

## Usage

``` r
compact_groups(group_indices)
```

## Arguments

- group_indices:

  An integer vector where element i contains the group index for
  entry i. A value of 0 indicates the entry belongs to no group.

## Value

A list with two elements:

- n_groups: The number of unique groups (N)

- group_indices: The compacted group indices (in range 1..N, with 0
  unchanged)

## Details

This function is useful when group indices have gaps (e.g., groups 5 and
10) and need to be renumbered to consecutive integers (1 and 2).

## Examples

``` r
if (FALSE) { # \dontrun{
# Groups 5 and 10 with gaps
group_indices <- c(5L, 10L, 5L, 0L, 10L)
result <- compact_groups(group_indices)
# result$n_groups is 2
# result$group_indices is c(1L, 2L, 1L, 0L, 2L)
} # }
```
