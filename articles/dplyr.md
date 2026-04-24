# dplyr backend

dafr exposes a [dplyr](https://dplyr.tidyverse.org) backend for the
**axes** of a daf. Each axis becomes a `tbl` whose rows are the axis
entries and whose columns are the vectors defined on that axis. Matrices
are intentionally *not* exposed through this interface — use the native
query DSL
([`vignette("queries", package = "dafr")`](https://tanaylab.github.io/dafr/articles/queries.md))
for those.

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
d <- example_cells_daf()
tbl(d, "cell")
#> <daf_axis_tbl>  axis: cell  [856 rows]
#> # A tibble: 6 × 3
#>   name                                donor experiment      
#>   <chr>                               <chr> <chr>           
#> 1 demux_07_12_20_1_AACAAGATCCATTTCA-1 N89   demux_07_12_20_1
#> 2 demux_07_12_20_1_AACGAAAGTCCAATCA-1 N84   demux_07_12_20_1
#> 3 demux_07_12_20_1_AAGACAAAGTTCCGTA-1 N86   demux_07_12_20_1
#> 4 demux_07_12_20_1_AGACTCATCTATTGTC-1 N84   demux_07_12_20_1
#> 5 demux_07_12_20_1_AGATAGACATTCCTCG-1 N89   demux_07_12_20_1
#> 6 demux_07_12_20_1_ATCGTAGTCCAGTGCG-1 N89   demux_07_12_20_1
```

## Familiar verbs

`filter`, `select`, `mutate`, `arrange`, `summarise`, `group_by`,
`distinct`, and `pull` all work as you’d expect.
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html)
materializes the tbl as a tibble.

``` r
tbl(d, "cell") |>
    filter(!is.na(donor)) |>
    group_by(donor) |>
    summarise(n = n()) |>
    arrange(desc(n))
#> <daf_axis_tbl>  axis: donor  [95 rows]
#> # A tibble: 6 × 2
#>   name      n
#>   <chr> <int>
#> 1 N104     34
#> 2 N99      27
#> 3 N96      21
#> 4 N118     20
#> 5 N113     19
#> 6 N159     19
```

## Write-back is explicit

[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) stores
computed columns in memory; it does not touch the daf. Persisting them
as vectors on the axis requires an explicit `compute(vectors = ...)` —
dafr never silently writes to the daf.

``` r
tbl(d, "cell") |>
    mutate(log_umis = log10(n_umis)) |>
    compute(vectors = "log_umis")
```

The daf backend reuses
[`dplyr::compute()`](https://dplyr.tidyverse.org/reference/compute.html)
(with an extra `vectors = ...` argument) rather than introducing a new
generic, so it doesn’t shadow
[`dplyr::compute()`](https://dplyr.tidyverse.org/reference/compute.html)
for dbplyr users.

[`compute()`](https://dplyr.tidyverse.org/reference/compute.html) errors
if the tbl has been filtered (partial row mask) — you cannot write back
a partial vector. A permuted-but-full row mask (e.g. after
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)) is
un-permuted to axis order before writing.

## Grouping that looks up an axis

If the grouping variable names an existing axis in the daf, the result
of [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
is a `daf_axis_tbl` keyed to that axis. You can keep piping dplyr verbs,
and later
[`compute()`](https://dplyr.tidyverse.org/reference/compute.html) the
derived columns back as vectors on that axis.

``` r
# "donor" is an axis; the summarise result is itself tbl(d, "donor")-like.
tbl(d, "cell") |>
    group_by(donor) |>
    summarise(mean_umis = mean(n_umis)) |>
    compute(vectors = "mean_umis")  # persists on the donor axis
```

If the grouping variable is just a vector (not an axis),
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
returns a plain tibble.

## Out of scope for v1

- Matrices. Use `Axis(...) |> LookupMatrix(...)` from the query DSL.
- Joins across axes.
- Window functions (`lag`, `cumsum`, etc.).
- DSL pushdown for `filter` / `summarise` (planned follow-up — verbs
  currently materialize via `get_vector`, which is cheap for memory daf
  but not optimal for large `FilesDaf` with selective filters).
