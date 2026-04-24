# Query DSL

## Two equivalent forms

Queries can be written as strings or as pipe-chain builder objects. The
following two expressions are equivalent:

``` r
head(d["@ cell : donor"])
#> [1] "N89" "N84" "N86" "N84" "N89" "N89"
head(d[Axis("cell") |> LookupVector("donor")])
#> [1] "N89" "N84" "N86" "N84" "N89" "N89"
```

## String form

``` r
# Scalar
d[". organism"]
#> [1] "human"
# Axis entries
head(d["@ cell"])
#> [1] "demux_07_12_20_1_AACAAGATCCATTTCA-1" "demux_07_12_20_1_AACGAAAGTCCAATCA-1"
#> [3] "demux_07_12_20_1_AAGACAAAGTTCCGTA-1" "demux_07_12_20_1_AGACTCATCTATTGTC-1"
#> [5] "demux_07_12_20_1_AGATAGACATTCCTCG-1" "demux_07_12_20_1_ATCGTAGTCCAGTGCG-1"
# Vector on axis
head(d["@ cell : donor"])
#> [1] "N89" "N84" "N86" "N84" "N89" "N89"
# Matrix
dim(d["@ cell @ gene :: UMIs"])
#> [1] 856 683
```

## Builder form

``` r
# Filtered axis entries — cells whose donor is "N89"
q <- Axis("cell") |> BeginMask("donor") |> IsEqual("N89") |> EndMask()
length(d[q])
#> [1] 14
```

## Reductions

``` r
# Per-gene mean UMI — reduce across the cell axis
per_gene_mean <- d[Axis("gene") |> Axis("cell") |> LookupMatrix("UMIs") |> ReduceToRow(Mean())]
head(per_gene_mean)
#> demux_07_12_20_1_AACAAGATCCATTTCA-1 demux_07_12_20_1_AACGAAAGTCCAATCA-1 
#>                            3.351391                            4.535871 
#> demux_07_12_20_1_AAGACAAAGTTCCGTA-1 demux_07_12_20_1_AGACTCATCTATTGTC-1 
#>                            2.411420                            4.131772 
#> demux_07_12_20_1_AGATAGACATTCCTCG-1 demux_07_12_20_1_ATCGTAGTCCAGTGCG-1 
#>                            2.641288                            3.929722
```

The equivalent string form uses `>-` (reduce-to-row) or `>|`
(reduce-to-column):

``` r
head(d["@ gene @ cell :: UMIs >- Mean"])
#> demux_07_12_20_1_AACAAGATCCATTTCA-1 demux_07_12_20_1_AACGAAAGTCCAATCA-1 
#>                            3.351391                            4.535871 
#> demux_07_12_20_1_AAGACAAAGTTCCGTA-1 demux_07_12_20_1_AGACTCATCTATTGTC-1 
#>                            2.411420                            4.131772 
#> demux_07_12_20_1_AGATAGACATTCCTCG-1 demux_07_12_20_1_ATCGTAGTCCAGTGCG-1 
#>                            2.641288                            3.929722
```

## Comparison + mask composition

``` r
# Donors older than 30 (the `age` vector lives on the `donor` axis)
q <- Axis("donor") |> BeginMask("age") |> IsGreater(30) |> EndMask()
length(d[q])
#> [1] 93
```

## See `?Axis`, `?LookupVector`, `?IsGreater` for the full builder list.
