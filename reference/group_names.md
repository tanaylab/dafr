# Deterministic names for groups of axis entries.

For each group in `entries_of_groups`, build a name from a stable hash
of the member entry names. Same-members =\> same name across sessions
and dafs.

## Usage

``` r
group_names(daf, axis, entries_of_groups, prefix)
```

## Arguments

- daf:

  A [DafReader](https://tanaylab.github.io/dafr/reference/DafReader.md).

- axis:

  Axis name whose entries the group indices reference.

- entries_of_groups:

  A list of integer vectors; each vector is the 1-based axis positions
  belonging to the group.

- prefix:

  Character scalar prepended to each name.

## Value

Character vector of length `length(entries_of_groups)`.

## See also

[`compact_groups()`](https://tanaylab.github.io/dafr/reference/compact_groups.md),
[`collect_group_members()`](https://tanaylab.github.io/dafr/reference/collect_group_members.md)

## Examples

``` r
d <- memory_daf()
add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
group_names(d, "cell", list(c(1L, 2L), c(3L, 4L)), prefix = "grp_")
#> [1] "grp_49e86dfe" "grp_0aad227c"
```
