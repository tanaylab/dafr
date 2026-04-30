# Reorder one or more axis entries in place.

Permutes the entries of one or more axes in the given daf, rewriting
every vector and matrix that depends on those axes. On `files_daf` the
operation is crash-recoverable via a `.reorder.backup/` directory of
hardlinks; on the next open with mode `"r+"` or `"w+"`, any in-progress
reorder is automatically rolled back to the pre-reorder state.

## Usage

``` r
reorder_axes(daf, ..., crash_counter = NULL)
```

## Arguments

- daf:

  A [DafWriter](https://tanaylab.github.io/dafr/reference/DafWriter.md).

- ...:

  Named permutations: `cell = perm_cell, gene = perm_gene`.

- crash_counter:

  Internal — for testing only. See `tick_crash_counter()` (not
  exported).

## Value

Invisibly the input `daf`.

## Details

Each `...` argument is a name = integer-permutation pair, where the
permutation is a 1-based vector of length `axis_length(daf, name)`.
After reorder, the entry at position `i` is the entry that was at
position `permutation[i]` before — i.e.
`new_entries[i] = old_entries[permutation[i]]`.

## Examples

``` r
d <- memory_daf()
add_axis(d, "cell", c("A", "B", "C"))
set_vector(d, "cell", "x", c(10, 20, 30))
reorder_axes(d, cell = c(3L, 1L, 2L))
axis_vector(d, "cell")
#> [1] "C" "A" "B"
get_vector(d, "cell", "x")
#>  C  A  B 
#> 30 10 20 
```
