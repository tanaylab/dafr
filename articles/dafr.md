# Getting Started with dafr

## What is a Daf?

A Daf (Data Axes Format) is a multi-axis container for biological /
scientific data. Think of it as a typed dictionary of scalars, per-axis
vectors, and per-axis-pair matrices, with cache invalidation and lazy
mmap-backed reads.

## Create a Daf

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
#>  @ internal              :<environment: 0x55b9155e9e80> 
#>  @ cache                 :<environment: 0x55b915603628> 
#>  @ axis_version_counter  :<environment: 0x55b915607ac0> 
#>  @ vector_version_counter:<environment: 0x55b9156077e8> 
#>  @ matrix_version_counter:<environment: 0x55b915607510>
```

## Reading data

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

## Queries

Queries let you compose reads. Two equivalent forms — a string DSL and
pipe-chain builders:

``` r
get_query(d, "@ cell : donor")
#> [1] "A" "B" "A"
get_query(d, ". organism")
#> [1] "human"
```

``` r
d[Axis("cell") |> LookupVector("donor")]
#> [1] "A" "B" "A"
```

## Data frames

``` r
get_dataframe(d, "cell")
#>    donor
#> c1     A
#> c2     B
#> c3     A
```

## Persistence

``` r
# Write to a directory:
fd <- files_daf(tempfile("dafr-"), mode = "w+", name = "persisted")
copy_all(d, fd)
```

## Example data

``` r
d2 <- example_cells_daf()
axes_set(d2)
#> [1] "cell"       "donor"      "experiment" "gene"
```
