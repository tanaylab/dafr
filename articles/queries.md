# Query DSL

dafr ships a small query language for composing reads against a daf.
This vignette is the practical tour; for the full operator grammar see
`vignette("query-dsl-reference", package = "dafr")`.

## Two equivalent forms

Every query has two surface syntaxes that compile to the same plan:

- **String DSL** - terse, good for ad-hoc work and scripts.
- **Builder pipe-chains** - composable R objects; good when query
  fragments are computed at runtime.

``` r

# String:
head(d["@ cell : donor"])
#> demux_07_12_20_1_AACAAGATCCATTTCA-1 demux_07_12_20_1_AACGAAAGTCCAATCA-1 
#>                               "N89"                               "N84" 
#> demux_07_12_20_1_AAGACAAAGTTCCGTA-1 demux_07_12_20_1_AGACTCATCTATTGTC-1 
#>                               "N86"                               "N84" 
#> demux_07_12_20_1_AGATAGACATTCCTCG-1 demux_07_12_20_1_ATCGTAGTCCAGTGCG-1 
#>                               "N89"                               "N89"
# Builder:
head(d[Axis("cell") |> LookupVector("donor")])
#> demux_07_12_20_1_AACAAGATCCATTTCA-1 demux_07_12_20_1_AACGAAAGTCCAATCA-1 
#>                               "N89"                               "N84" 
#> demux_07_12_20_1_AAGACAAAGTTCCGTA-1 demux_07_12_20_1_AGACTCATCTATTGTC-1 
#>                               "N86"                               "N84" 
#> demux_07_12_20_1_AGATAGACATTCCTCG-1 demux_07_12_20_1_ATCGTAGTCCAGTGCG-1 
#>                               "N89"                               "N89"
```

[`canonical_query()`](https://tanaylab.github.io/dafr/reference/canonical_query.md)
and
[`parse_query()`](https://tanaylab.github.io/dafr/reference/parse_query.md)
let you inspect what a string compiles to:

``` r

canonical_query("@ cell : donor")
#> [1] "@ cell : donor"
```

## Lookups

The smallest queries look up a single piece of data. Each is keyed by
how many axes you push onto the stack before the property.

``` r

# Scalar (zero axes):
d[". organism"]
#> [1] "human"
# Vector (one axis):
head(d["@ donor : age"])
#> N16 N17 N18 N59 N79 N83 
#>  61  73  71  75  66  42
# Matrix (two axes):
dim(d["@ cell @ gene :: UMIs"])
#> [1] 856 683
```

## Masks: filtering an axis

Wrap a comparison in `[ ... ]` (string) or `BeginMask(...)` /
[`EndMask()`](https://tanaylab.github.io/dafr/reference/EndMask.md)
(builder) to keep only the entries that pass:

``` r

# Donors older than 60 - string and builder are equivalent:
length(d["@ donor [ age > 60 ]"])
#> [1] 59
length(d[Axis("donor") |> BeginMask("age") |> IsGreater(60) |> EndMask()])
#> [1] 59
```

Masks compose with `&` (AND), `|` (OR), `^` (XOR), each with an optional
`!` for negation. Combinators are left-to-right, not by precedence -
parenthesize via nested `[ ... ]` blocks if you need grouping.

``` r

# Female donors over 60:
length(d["@ donor [ age > 60 & sex = female ]"])
#> [1] 30
length(d[Axis("donor") |>
    BeginMask("age") |> IsGreater(60) |>
    AndMask("sex") |> IsEqual("female") |>
    EndMask()])
#> [1] 30
```

## Element-wise transforms

`Log`, `Abs`, `Clamp`, `Round`, `Convert`, etc. transform the values in
place. In the string DSL they follow a `%` prefix.

``` r

# log2(age + 1):
head(d["@ donor : age % Log base 2 eps 1"])
#>      N16      N17      N18      N59      N79      N83 
#> 5.954196 6.209453 6.169925 6.247928 6.066089 5.426265
head(d[Axis("donor") |> LookupVector("age") |> Log(base = 2, eps = 1)])
#>      N16      N17      N18      N59      N79      N83 
#> 5.954196 6.209453 6.169925 6.247928 6.066089 5.426265
```

## Reductions

A reduction collapses one dimension. For matrices, `>-` reduces **to a
row** (across the column axis), `>|` reduces **to a column** (across the
row axis); the builder forms are
[`ReduceToRow()`](https://tanaylab.github.io/dafr/reference/ReduceToRow.md)
and
[`ReduceToColumn()`](https://tanaylab.github.io/dafr/reference/ReduceToColumn.md).
For vectors the reduction is `>>` and yields a scalar.

``` r

# Per-gene mean UMIs (reduce across cells):
head(d["@ gene @ cell :: UMIs >- Mean"])
#> demux_07_12_20_1_AACAAGATCCATTTCA-1 demux_07_12_20_1_AACGAAAGTCCAATCA-1 
#>                            3.351391                            4.535871 
#> demux_07_12_20_1_AAGACAAAGTTCCGTA-1 demux_07_12_20_1_AGACTCATCTATTGTC-1 
#>                            2.411420                            4.131772 
#> demux_07_12_20_1_AGATAGACATTCCTCG-1 demux_07_12_20_1_ATCGTAGTCCAGTGCG-1 
#>                            2.641288                            3.929722
head(d[Axis("gene") |> Axis("cell") |> LookupMatrix("UMIs") |> ReduceToRow(Mean())])
#> demux_07_12_20_1_AACAAGATCCATTTCA-1 demux_07_12_20_1_AACGAAAGTCCAATCA-1 
#>                            3.351391                            4.535871 
#> demux_07_12_20_1_AAGACAAAGTTCCGTA-1 demux_07_12_20_1_AGACTCATCTATTGTC-1 
#>                            2.411420                            4.131772 
#> demux_07_12_20_1_AGATAGACATTCCTCG-1 demux_07_12_20_1_ATCGTAGTCCAGTGCG-1 
#>                            2.641288                            3.929722
```

``` r

# Library size per cell (reduce across genes):
head(d["@ cell @ gene :: UMIs >| Sum"])
#> demux_07_12_20_1_AACAAGATCCATTTCA-1 demux_07_12_20_1_AACGAAAGTCCAATCA-1 
#>                                2289                                3098 
#> demux_07_12_20_1_AAGACAAAGTTCCGTA-1 demux_07_12_20_1_AGACTCATCTATTGTC-1 
#>                                1647                                2822 
#> demux_07_12_20_1_AGATAGACATTCCTCG-1 demux_07_12_20_1_ATCGTAGTCCAGTGCG-1 
#>                                1804                                2684
```

``` r

# Vector -> scalar:
d["@ donor : age >> Mean"]
#> [1] 64.41053
```

## GroupBy

`/` on a vector groups by another property of the same length and
applies a reduction per group. `-/` / `|/` do the same for matrix rows /
columns.

``` r

# Mean donor age, split by sex:
d["@ donor : age / sex >> Mean"]
#>   female     male 
#> 63.19231 65.88372
d[Axis("donor") |> LookupVector("age") |> GroupBy("sex", AsAxis()) |> Mean()]
#>   female     male 
#> 63.19231 65.88372
```

``` r

# Mean UMIs per donor, per gene (group cell rows by their donor):
m <- d["@ cell @ gene :: UMIs -/ donor >- Mean"]
dim(m)
#> [1]  95 683
m[1:3, 1:3]
#>         RPL22     PARK7      ENO1
#> N100 6.857143 0.3571429 0.8571429
#> N101 6.375000 0.4375000 1.1875000
#> N102 3.857143 0.2857143 0.4285714
```

## IfMissing: safe lookups

[`IfMissing()`](https://tanaylab.github.io/dafr/reference/IfMissing.md)
lets a vector / matrix lookup fall back to a default when the property
doesn’t exist. Useful for queries that survive across dafs with slightly
different schemas.

``` r

head(d[Axis("gene") |> LookupVector("absent_property") |> IfMissing(-1)])
#>  RPL22  PARK7   ENO1  PRDM2 HP1BP3  CDC42 
#>     -1     -1     -1     -1     -1     -1
```

## Tips

- Use `parse_query(s)` to see the operator list a string compiles to;
  this is the easiest way to learn the grammar by analogy.
- Builder fragments are ordinary R objects: store them in variables,
  build them in loops, pass them around. The expression
  `Axis("cell") |> LookupVector("donor")` is a `DafrQuery` you can index
  with later.
- [`?Axis`](https://tanaylab.github.io/dafr/reference/Axis.md),
  [`?LookupVector`](https://tanaylab.github.io/dafr/reference/LookupVector.md),
  [`?Mean`](https://tanaylab.github.io/dafr/reference/Mean.md),
  [`?GroupBy`](https://tanaylab.github.io/dafr/reference/GroupBy.md)
  (etc.) have the full per-operator detail; the reference grouping is
  also in the pkgdown “Query builders” section.
