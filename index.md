# dafr - Data in Axes in Formats for R (native)

`dafr` is a native R + C++ implementation of the
[DataAxesFormats](https://github.com/tanaylab/DataAxesFormats.jl) (DAF)
data model: a uniform, self-describing container for 1D and 2D data
arranged along a set of named axes. Think of it as a generalisation of
[AnnData](https://github.com/scverse/anndata) with scalars, per-axis
vectors, per-axis-pair matrices, disk-backed persistence, and a
composable query DSL.

This package is a pure R + C++ port of the Julia reference
implementation — **no Julia install is required**.

## Installation

``` r

# Install from GitHub:
remotes::install_github("tanaylab/dafr")

# Or from a local checkout:
# R CMD INSTALL /path/to/dafr
```

This package has no Julia dependency. It is pure R + C++.

## Native advantages

- No Julia install required — pure R + C++ (no JuliaCall copy tax).
- Mmap-backed reads via `mmap_dgCMatrix` / `mmap_int` / `mmap_lgl` /
  `mmap_real`.
- OpenMP-parallel kernels for Sum / Mean / Var / Mode / Quantile /
  GeoMean.
- `register_eltwise` / `register_reduction` for user-defined query ops.
- Pipe-chain builders:
  `daf[Axis("cell") |> LookupVector("age") |> IsGreater(2)]`.
- AnnData interop via `DafAnnData` + `h5ad_as_daf` / `daf_as_h5ad`.

## Usage

``` r

library(dafr)
#> 
#> Attaching package: 'dafr'
#> The following object is masked from 'package:graphics':
#> 
#>     Axis

# Create a DAF object in memory
daf <- memory_daf("example")

# Add axes
add_axis(daf, "obs", c("cell1", "cell2", "cell3"))
add_axis(daf, "var", c("gene1", "gene2"))

# Add vector data
set_vector(daf, "obs", "score", c(0.1, 0.5, 0.9))

# Add matrix data
mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
set_matrix(daf, "obs", "var", "counts", mat)

# Access data
get_vector(daf, "obs", "score")
#> cell1 cell2 cell3 
#>   0.1   0.5   0.9
get_matrix(daf, "obs", "var", "counts")
#>       gene1 gene2
#> cell1     1     4
#> cell2     2     5
#> cell3     3     6

# Get a dataframe view
get_dataframe(daf, "obs")
#>       score
#> cell1   0.1
#> cell2   0.5
#> cell3   0.9
```

## Querying Data

`dafr` provides two equivalent query interfaces: a compact string DSL
and pipe-composable builders. The `[` operator on any `DafReader`
accepts either form.

``` r

# Create a sample dataset
daf <- memory_daf("query_examples")
add_axis(daf, "cell", c("A", "B", "C"))
add_axis(daf, "gene", c("X", "Y", "Z"))
set_vector(daf, "cell", "age", c(10, 50, 70))
set_vector(daf, "cell", "type", c("T", "B", "NK"))
set_scalar(daf, "version", "1.0")
set_matrix(daf, "cell", "gene", "counts", matrix(1:9, nrow = 3, ncol = 3))

# Scalar
daf[". version"]
#> [1] "1.0"

# Axes
daf["@ ?"]
#> [1] "cell" "gene"
daf["@ cell : ?"]
#> [1] "age"  "type"

# Vector data
daf["@ cell : age"]
#> [1] 10 50 70

# Matrix data
daf["@ cell @ gene :: counts"]
#>      [,1] [,2] [,3]
#> [1,]    1    4    7
#> [2,]    2    5    8
#> [3,]    3    6    9

# Filter axis by property
daf["@ cell [ age > 20 ]"]
#> [1] "B" "C"

# Transform with eltwise op
daf["@ cell : age % Abs"]
#> [1] 10 50 70

# Pipe-chain builder form
daf[Axis("cell") |> LookupVector("age")]
#> [1] 10 50 70

# Composed builder: mask + vector lookup
daf[
    Axis("cell") |>
        BeginMask("age") |>
        IsGreater(20) |>
        EndMask()
]
#> [1] "B" "C"
```

The query syntax follows a simple pattern:

- `@ axis` selects an axis
- `. property` retrieves a scalar
- `: property` retrieves a vector property
- `:: property` retrieves a matrix property
- `% operation` applies an element-wise operation
- `>> reduction` applies a reduction operation
- `[ property comparison value ]` filters by a property

See the upstream Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html)
for the full query language reference — the native parser is
semantically equivalent.

## AnnData interop

A Daf object can be exposed as an `AnnData`-shaped facade with the
familiar `X` / `obs` / `var` / `layers` / `uns` / `obs_names` /
`var_names` / `shape` properties:

``` r

d <- example_cells_daf()
ann <- as_anndata(d)

ann$n_obs
#> [1] 856
ann$n_vars
#> [1] 683
dim(ann$X)
#> [1] 856 683
head(ann$obs_names)
#> [1] "demux_07_12_20_1_AACAAGATCCATTTCA-1" "demux_07_12_20_1_AACGAAAGTCCAATCA-1"
#> [3] "demux_07_12_20_1_AAGACAAAGTTCCGTA-1" "demux_07_12_20_1_AGACTCATCTATTGTC-1"
#> [5] "demux_07_12_20_1_AGATAGACATTCCTCG-1" "demux_07_12_20_1_ATCGTAGTCCAGTGCG-1"
head(ann$obs)
#>                                     donor       experiment
#> demux_07_12_20_1_AACAAGATCCATTTCA-1   N89 demux_07_12_20_1
#> demux_07_12_20_1_AACGAAAGTCCAATCA-1   N84 demux_07_12_20_1
#> demux_07_12_20_1_AAGACAAAGTTCCGTA-1   N86 demux_07_12_20_1
#> demux_07_12_20_1_AGACTCATCTATTGTC-1   N84 demux_07_12_20_1
#> demux_07_12_20_1_AGATAGACATTCCTCG-1   N89 demux_07_12_20_1
#> demux_07_12_20_1_ATCGTAGTCCAGTGCG-1   N89 demux_07_12_20_1
```

The facade is read-only; writes (`ann$X <- ...`) raise an error. Modify
the underlying Daf directly if you need to change data.

Round-trip h5ad I/O is supported (requires the `hdf5r` Suggests dep).
Sparse `X`, categorical `obs` / `var` columns, nested `uns`, and `obsm`
/ `varm` all round-trip:

``` r

d  <- h5ad_as_daf("path/to/file.h5ad")
daf_as_h5ad(d, "out.h5ad", overwrite = TRUE)
```

## dplyr backend

`tbl(daf, axis)` returns a lazy `daf_axis_tbl` where rows are axis
entries and columns are per-axis vectors. Most dplyr verbs work
natively:

``` r

library(dplyr)

d <- example_cells_daf()

tbl(d, "cell") |>
    filter(donor == "N89") |>
    select(name, donor, experiment) |>
    collect() |>
    head()
#> # A tibble: 6 x 3
#>                                  name donor       experiment
#> 1 demux_07_12_20_1_AACAAGATCCATTTCA-1   N89 demux_07_12_20_1
#> 2 demux_07_12_20_1_AGATAGACATTCCTCG-1   N89 demux_07_12_20_1
#> 3 demux_07_12_20_1_ATCGTAGTCCAGTGCG-1   N89 demux_07_12_20_1
#> 4 demux_07_12_20_1_CACAGGCGTCCTACAA-1   N89 demux_07_12_20_1
#> 5 demux_07_12_20_1_CCTACGTAGCCAACCC-1   N89 demux_07_12_20_1
#> 6 demux_07_12_20_1_GAAGGGTGTCCCTGAG-1   N89 demux_07_12_20_1

tbl(d, "cell") |>
    count(experiment, sort = TRUE) |>
    collect() |>
    head()
#> # A tibble: 6 x 2
#>               name  n
#> 1 demux_28_12_20_2 72
#> 2 demux_28_12_20_1 58
#> 3 demux_21_02_21_2 51
#> 4 demux_22_02_21_2 47
#> 5 demux_01_03_21_1 45
#> 6 demux_07_12_20_2 42
```

Supported verbs: `filter`, `select`, `mutate`, `arrange`, `summarise`,
`group_by` / `ungroup`, `distinct`, `pull`, `collect`, `as_tibble`,
`count` / `tally` / `add_count` / `add_tally`.

When a grouping variable names another axis already in the daf,
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
and [`count()`](https://dplyr.tidyverse.org/reference/count.html)
**auto-tie-back** to a lazy `daf_axis_tbl` on that axis — so the
pipeline can continue. Axis entries then appear in the `name` column
(the convention for the axis-identity column). Example:

``` r

# `donor` is an axis of example_cells_daf(); count() re-anchors onto it.
tbl(d, "cell") |>
    count(donor, sort = TRUE) |>
    class()   # daf_axis_tbl + tbl_df
#> [1] "daf_axis_tbl"
```

Mutations are accumulated lazily and can be written back with
`dplyr::compute(tbl, vectors = c(...))`:

``` r

# Lazy pipeline...
updated <- tbl(d, "cell") |>
    mutate(donor_upper = toupper(donor))

# ...materialise the new vector on the underlying daf:
dplyr::compute(updated, vectors = "donor_upper")
```

Matrices are out of scope — use the native query DSL for those.

## Key Features

- Full Daf data model: scalars, per-axis vectors, per-axis-pair
  matrices.
- In-memory (`memory_daf`) and on-disk (`files_daf`) stores.
- Zero-copy mmap-backed reads for vectors and sparse matrices on
  read-only `FilesDaf`.
- OpenMP-parallel reduction kernels (Sum, Mean, Var, Mode, Quantile,
  GeoMean).
- Views and adapters for zero-copy subsetting / renaming.
- Chaining repositories for efficient overlay / reuse of data.
- Contracts for explicit computation input / output validation.
- Explicit support for grouped axes and group helpers.
- Query DSL (string form + pipe-chain builders) with user-extensible ops
  (`register_eltwise`, `register_reduction`).
- AnnData (`h5ad`) import / export.
- Control over data layout and support for both dense and sparse
  matrices.

## Status

First public release: `0.1.0`. See `NEWS.md` for headline features and
known gaps. Comments, bug reports, and PRs are welcome.

## Roadmap

Tracked in `NEWS.md`. Notable near-term items:

- HDF5-backed `h5df` store.
- CRAN submission.

## License (MIT)

Copyright (c) 2025-2026 Weizmann Institute of Science

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
“Software”), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
