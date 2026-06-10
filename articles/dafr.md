# Getting Started with dafr

## What is a Daf?

A **Daf** (Data Axes Format) is a typed, multi-axis container for
scientific data. Each daf holds:

- **Axes** - named lists of entries (e.g. `cell`, `gene`, `donor`).
- **Scalars** - single values (e.g. `organism = "human"`).
- **Vectors** - one value per entry on a chosen axis.
- **Matrices** - one value per pair of entries on two chosen axes.

Think of it as AnnData generalised to any number of axes, with typed
scalars, on-disk persistence in several formats, and a query DSL for
composing reads.

## Build a daf in memory

``` r

d <- memory_daf(name = "demo")
add_axis(d, "cell", c("c1", "c2", "c3"))
add_axis(d, "gene", c("g1", "g2"))
set_scalar(d, "organism", "human")
set_vector(d, "cell", "donor", c("A", "B", "A"))
set_matrix(d, "cell", "gene", "UMIs",
           matrix(1:6, nrow = 3, ncol = 2))
print(d)
#> <dafr::MemoryDaf>
#>  @ name                  : chr "demo"
#>  @ internal              :<environment: 0x564b8c250ce0> 
#>  @ cache                 :<environment: 0x564b8c28dcb0> 
#>  @ axis_version_counter  :<environment: 0x564b8c292110> 
#>  @ vector_version_counter:<environment: 0x564b8c291e38> 
#>  @ matrix_version_counter:<environment: 0x564b8c291b60>
```

`description(d)` produces a more detailed dump of axes, vectors,
matrices, and scalars, useful for inspecting unfamiliar dafs:

``` r

cat(description(d))
#> name: demo
#> type: MemoryDaf
#> scalars:
#>   organism: "human"
#> axes:
#>   cell: 3 entries
#>   gene: 2 entries
#> vectors:
#>   cell:
#>     donor
#> matrices:
#>   cell,gene:
#>     UMIs
```

## Read data back

``` r

get_scalar(d, "organism")
#> [1] "human"
get_vector(d, "cell", "donor")
#>  c1  c2  c3 
#> "A" "B" "A"
get_matrix(d, "cell", "gene", "UMIs")
#>    g1 g2
#> c1  1  4
#> c2  2  5
#> c3  3  6
```

`scalars_set(d)`, `vectors_set(d, axis)`, and `matrices_set(d, a, b)`
list the property names present on the corresponding container.

## Queries

Queries let you compose reads. Two equivalent forms - a string DSL and
pipe-chain builders - share the same execution engine:

``` r

d[". organism"]
#> [1] "human"
d[Axis("cell") |> LookupVector("donor")]
#>  c1  c2  c3 
#> "A" "B" "A"
```

``` r

# Mean UMI count per cell (reduce across the gene axis):
d["@ cell @ gene :: UMIs >| Mean"]
#>  c1  c2  c3 
#> 2.5 3.5 4.5
```

See
[`vignette("queries", package = "dafr")`](https://tanaylab.github.io/dafr/articles/queries.md)
for the practical tour and
`vignette("query-dsl-reference", package = "dafr")` for the full
operator grammar.

## Data frames and dplyr

[`get_dataframe()`](https://tanaylab.github.io/dafr/reference/get_dataframe.md)
materialises an axis-keyed slice as a `tibble`:

``` r

get_dataframe(d, "cell")
#>    donor
#> c1     A
#> c2     B
#> c3     A
```

The dplyr backend lets you treat any axis as a `tbl` and pipe verbs:

``` r

library(dplyr, warn.conflicts = FALSE)
tbl(d, "cell") |>
    group_by(donor) |>
    summarise(n = n())
#> # A tibble: 2 × 2
#>   donor     n
#>   <chr> <int>
#> 1 A         2
#> 2 B         1
```

See
[`vignette("dplyr", package = "dafr")`](https://tanaylab.github.io/dafr/articles/dplyr.md)
for the supported verbs and write-back semantics.

## Persistence

A daf can be persisted in three on-disk shapes - pick the one that
matches how the data needs to travel.

``` r

# files_daf: one file per property, mmap-backed reads.
path <- tempfile("dafr-")
fd <- files_daf(path, mode = "w+", name = "persisted")
copy_all(fd, d)  # destination first, then source
list.files(path, recursive = TRUE)
#>  [1] "axes/cell.txt"                "axes/gene.txt"               
#>  [3] "axes/metadata.json"           "daf.json"                    
#>  [5] "matrices/cell/gene/UMIs.data" "matrices/cell/gene/UMIs.json"
#>  [7] "matrices/gene/cell/UMIs.data" "matrices/gene/cell/UMIs.json"
#>  [9] "metadata.zip"                 "scalars/organism.json"       
#> [11] "vectors/cell/donor.json"      "vectors/cell/donor.txt"
```

``` r

# Reopen read-only:
fd2 <- files_daf(path, mode = "r")
get_scalar(fd2, "organism")
#> [1] "human"
get_matrix(fd2, "cell", "gene", "UMIs")
#>    g1 g2
#> c1  1  4
#> c2  2  5
#> c3  3  6
```

[`zarr_daf()`](https://tanaylab.github.io/dafr/reference/zarr_daf.md)
(directory or `.daf.zarr.zip`) and
[`http_daf()`](https://tanaylab.github.io/dafr/reference/HttpDaf.md)
(read-only HTTP) round-trip the same data; see
[`vignette("zarr", package = "dafr")`](https://tanaylab.github.io/dafr/articles/zarr.md).

## Where to go next

- **Query DSL** -
  [`vignette("queries", package = "dafr")`](https://tanaylab.github.io/dafr/articles/queries.md).
- **dplyr backend** -
  [`vignette("dplyr", package = "dafr")`](https://tanaylab.github.io/dafr/articles/dplyr.md).
- **AnnData / h5ad interop** -
  [`vignette("anndata", package = "dafr")`](https://tanaylab.github.io/dafr/articles/anndata.md).
- **Zarr & HTTP storage** -
  [`vignette("zarr", package = "dafr")`](https://tanaylab.github.io/dafr/articles/zarr.md).
- **Native performance notes** -
  [`vignette("native-performance", package = "dafr")`](https://tanaylab.github.io/dafr/articles/native-performance.md).
- **Larger building blocks** - chains (`chain_reader`), views (`viewer`,
  `VIEW_ALL_DATA`), contracts (`Contract`, `verify_input`), and
  computations (`computation`, `adapter`) all have detailed help pages
  in the reference.

## Example data

``` r

d2 <- example_cells_daf()
axes_set(d2)
#> [1] "cell"       "donor"      "experiment" "gene"
```
