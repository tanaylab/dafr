# Evaluate a query against a daf reader.

The query language follows DataAxesFormats.jl's `queries.jl` exactly;
the examples below mirror its jldoctest blocks one-for-one so any
regression is caught by `R CMD check`.

## Usage

``` r
get_query(daf, query_string)
```

## Arguments

- daf:

  A `DafReader`.

- query_string:

  A query string (character scalar) or a
  [DafrQuery](https://tanaylab.github.io/dafr/reference/DafrQuery.md)
  object produced by the query builders (e.g.
  `Axis("cell") |> LookupVector("donor")`).

## Value

A scalar, vector, matrix, names set, or NULL if missing.

## Examples

``` r
cells <- example_cells_daf()
metacells <- example_metacells_daf()
chain <- example_chain_daf()

# --- Names queries (jl:92-140) -------------------------------------------
cells[". ?"]                            # scalar names
#> [1] "organism"  "reference"
cells["@ ?"]                            # axis names
#> [1] "cell"       "donor"      "experiment" "gene"      
cells["@ gene : ?"]                     # vector names on gene
#> [1] "is_lateral"
cells["@ cell @ gene :: ?"]             # matrix names on cell x gene
#> [1] "UMIs"

# --- Reductions to scalar (jl:162, 174) ----------------------------------
cells["@ gene : is_lateral >> Sum type Int64"]
#> [1] 438
cells["@ cell @ gene :: UMIs >> Sum type Int64"]
#> [1] 1171936

# --- Axis vectors and masks (jl:195, 254, 293) ---------------------------
head(cells["@ experiment"])
#> [1] "demux_01_02_21_1" "demux_01_02_21_2" "demux_01_03_21_1" "demux_04_01_21_1"
#> [5] "demux_04_01_21_2" "demux_07_03_21_1"
head(cells["@ gene [ ! is_lateral ]"])
#> [1] "ENO1"    "PRDM2"   "HP1BP3"  "HNRNPR"  "RSRP1"   "KHDRBS1"
head(cells["@ donor [ age > 60 & sex = male ]"])
#> [1] "N16" "N17" "N59" "N86" "N88" "N91"

# --- Vector lookup (jl:332) ----------------------------------------------
metacells["@ metacell : type"]
#>   M1671.28   M2357.20   M2169.56   M2576.86   M1440.15    M756.63    M412.08 
#>      "MPP"      "MPP" "MEBEMP-L" "MEBEMP-E"      "MPP" "MEBEMP-E" "memory-B" 

# --- Matrix-by-axis column slice (jl:357) --------------------------------
head(metacells["@ gene :: fraction @ metacell = M412.08"])
#>        RPL22        PARK7         ENO1        PRDM2       HP1BP3        CDC42 
#> 3.735807e-03 6.505314e-05 4.222280e-05 1.514862e-04 1.209899e-04 1.763767e-04 

# --- Square matrix slices (jl:386, 408) ----------------------------------
metacells["@ metacell :: edge_weight @| M412.08"]
#> M1671.28 M2357.20 M2169.56 M2576.86 M1440.15  M756.63  M412.08 
#>      0.0      0.0      0.0      0.0      0.5      0.1      0.0 
metacells["@ metacell :: edge_weight @- M412.08"]
#> M1671.28 M2357.20 M2169.56 M2576.86 M1440.15  M756.63  M412.08 
#>      0.0      0.0      0.1      0.0      0.0      0.9      0.0 

# --- Chained vector lookup (jl:448) --------------------------------------
metacells["@ metacell : type : color"]
#>    M1671.28    M2357.20    M2169.56    M2576.86    M1440.15     M756.63 
#>      "gold"      "gold"      "plum"   "#eebb6e"      "gold"   "#eebb6e" 
#>     M412.08 
#> "steelblue" 

# --- Eltwise + comparator on vector (jl:468, 498) ------------------------
head(cells["@ donor : age % Clamp min 40 max 60 type Int64"])
#> N16 N17 N18 N59 N79 N83 
#>  60  60  60  60  60  42 
head(cells["@ donor : age > 60"])
#>   N16   N17   N18   N59   N79   N83 
#>  TRUE  TRUE  TRUE  TRUE  TRUE FALSE 

# --- AsAxis explicit and named (jl:548, 566) -----------------------------
metacells["@ metacell : type =@ : color"]
#>    M1671.28    M2357.20    M2169.56    M2576.86    M1440.15     M756.63 
#>      "gold"      "gold"      "plum"   "#eebb6e"      "gold"   "#eebb6e" 
#>     M412.08 
#> "steelblue" 
metacells["@ metacell : type =@ type : color"]
#>    M1671.28    M2357.20    M2169.56    M2576.86    M1440.15     M756.63 
#>      "gold"      "gold"      "plum"   "#eebb6e"      "gold"   "#eebb6e" 
#>     M412.08 
#> "steelblue" 

# --- Group-by + chain + reduction (jl:598, 620) --------------------------
chain["@ cell : donor : age / metacell ?? : type >> Mean"]
#> MEBEMP-E      MPP memory-B MEBEMP-L 
#> 63.97674 64.23795 62.30769 63.95238 
chain[paste(
    "@ cell [ metacell ?? : type != memory-B ] :",
    "donor : age / metacell : type =@ >> Mean || 0"
)]
#> memory-B MEBEMP-E MEBEMP-L      MPP 
#>  0.00000 63.97674 63.95238 64.23795 

# --- Matrix reductions to a vector (jl:647, 668) -------------------------
metacells["@ metacell @ gene :: fraction >| Max"]
#>   M1671.28   M2357.20   M2169.56   M2576.86   M1440.15    M756.63    M412.08 
#> 0.02332097 0.02334252 0.02192346 0.02367191 0.02276774 0.02491207 0.02849361 
head(metacells["@ metacell @ gene :: fraction >- Max"])
#>        RPL22        PARK7         ENO1        PRDM2       HP1BP3        CDC42 
#> 0.0047409558 0.0001541991 0.0005338871 0.0001514862 0.0002482063 0.0002078475 

# --- Full matrix lookup (jl:709) -----------------------------------------
dim(cells["@ cell @ gene :: UMIs"])
#> [1] 856 683

# --- Count-by + chained vector (jl:754, 786) -----------------------------
cells["@ cell : experiment * donor : sex"]
#>                   b
#> a                  female male
#>   demux_01_02_21_1     23   14
#>   demux_01_02_21_2     10   26
#>   demux_01_03_21_1     18   27
#>   demux_04_01_21_1     19   22
#>   demux_04_01_21_2      6   18
#>   demux_07_03_21_1     10   22
#>   demux_07_03_21_2     13   27
#>   demux_07_12_20_1      6   17
#>   demux_07_12_20_2     32   10
#>   demux_11_01_21_1     25    6
#>   demux_11_01_21_2     21   20
#>   demux_11_04_21_1     13   19
#>   demux_11_04_21_2     13    9
#>   demux_21_01_21_1      4   27
#>   demux_21_01_21_2     17   11
#>   demux_21_02_21_1     18    5
#>   demux_21_02_21_2      9   42
#>   demux_21_12_20_1     30    5
#>   demux_21_12_20_2      0   38
#>   demux_22_02_21_1     18    9
#>   demux_22_02_21_2     28   19
#>   demux_28_12_20_1     24   34
#>   demux_28_12_20_2     63    9
cells["@ cell : experiment =@ * donor : sex"]
#>                   b
#> a                  female male
#>   demux_01_02_21_1     23   14
#>   demux_01_02_21_2     10   26
#>   demux_01_03_21_1     18   27
#>   demux_04_01_21_1     19   22
#>   demux_04_01_21_2      6   18
#>   demux_07_03_21_1     10   22
#>   demux_07_03_21_2     13   27
#>   demux_07_12_20_1      6   17
#>   demux_07_12_20_2     32   10
#>   demux_11_01_21_1     25    6
#>   demux_11_01_21_2     21   20
#>   demux_11_04_21_1     13   19
#>   demux_11_04_21_2     13    9
#>   demux_21_01_21_1      4   27
#>   demux_21_01_21_2     17   11
#>   demux_21_02_21_1     18    5
#>   demux_21_02_21_2      9   42
#>   demux_21_12_20_1     30    5
#>   demux_21_12_20_2      0   38
#>   demux_22_02_21_1     18    9
#>   demux_22_02_21_2     28   19
#>   demux_28_12_20_1     24   34
#>   demux_28_12_20_2     63    9

# --- Matrix eltwise + group rows by (jl:830, 866, 889) -------------------
dim(metacells["@ metacell @ gene :: fraction % Log base 2 eps 1e-5"])
#> [1]   7 683
dim(metacells["@ metacell @ gene :: fraction -/ type >- Mean"])
#> [1]   4 683
dim(metacells["@ metacell @ gene :: fraction -/ type =@ >- Mean"])
#> [1]   4 683
```
