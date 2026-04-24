# Invert group-index assignment into per-group entry lists.

For each non-zero group in `group_indices`, returns the integer
positions that belong to it. Entries with index 0 are omitted.

## Usage

``` r
collect_group_members(group_indices)
```

## Arguments

- group_indices:

  Integer vector (or coercible).

## Value

A list of integer vectors. Length equals `max(group_indices)`.

## See also

[`compact_groups()`](https://tanaylab.github.io/dafr/reference/compact_groups.md),
[`group_names()`](https://tanaylab.github.io/dafr/reference/group_names.md)

## Examples

``` r
collect_group_members(c(1L, 2L, 1L, 0L, 2L))
#> [[1]]
#> [1] 1 3
#> 
#> [[2]]
#> [1] 2 5
#> 
# list(c(1, 3), c(2, 5))
```
