# Renumber group indices to be dense in 1..N.

Given a vector where element `i` is the group index for entry `i` (0
denotes "no group"), return a list with `n_groups` (the number of unique
non-zero groups) and `group_indices` (the renumbered indices, using
first-seen order). Zeros are preserved.

## Usage

``` r
compact_groups(group_indices)
```

## Arguments

- group_indices:

  Integer vector (or coercible).

## Value

`list(n_groups = integer(1), group_indices = integer(N))`.

## See also

[`collect_group_members()`](https://tanaylab.github.io/dafr/reference/collect_group_members.md),
[`group_names()`](https://tanaylab.github.io/dafr/reference/group_names.md)

## Examples

``` r
compact_groups(c(5L, 10L, 5L, 0L, 10L))
#> $n_groups
#> [1] 2
#> 
#> $group_indices
#> [1] 1 2 1 0 2
#> 
# $n_groups = 2; $group_indices = c(1, 2, 1, 0, 2)
```
