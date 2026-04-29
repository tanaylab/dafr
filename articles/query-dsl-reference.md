# Query DSL reference

This article is the normative reference for the `dafr` query language.
The semantics follow the Julia reference implementation
[`DataAxesFormats.jl`](https://github.com/tanaylab/DataAxesFormats.jl);
the short [Query DSL
vignette](https://tanaylab.github.io/dafr/queries.md) is a tutorial,
this one is the specification.

The DSL has two surface forms:

- a **string form** (`daf["@ cell : donor"]`), tokenised and parsed.
- a **builder form** — pipe-composable R objects
  (`daf[Axis("cell") |> LookupVector("donor")]`).

The two forms are interchangeable: any string is parsed into the same
AST the builders produce, so examples below give both forms for the
first occurrence of each phrase and then use whichever is clearer.

## Execution model

A query is a sequence of operators that is not executed one operator at
a time. Instead, the parser groups operators into **phrases**, where
each phrase is a stack-rewriting step:

1.  The query state is a stack, starting empty.
2.  Each phrase matches a pattern on the top of the stack (for example,
    a `LookupVector` phrase needs an axis specification on top).
3.  The phrase pops the matching top elements, performs its operation,
    and pushes zero or more new elements.
4.  When all operators have been consumed, the stack must hold exactly
    one result; that result is what the query returns.

The grouping-into-phrases means an operator’s meaning depends on the
phrase it participates in. Where a table of operators is given below,
the **phrase** column indicates which phrase the operator appears in.

## Operator table

| Operator | Builder                                                                               | Phrase / meaning                                                                             |
|:---------|:--------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------|
| `@`      | [`Axis()`](https://tanaylab.github.io/dafr/reference/Axis.md)                         | Introduce an axis onto the stack.                                                            |
| `=@`     | [`AsAxis()`](https://tanaylab.github.io/dafr/reference/AsAxis.md)                     | Treat subsequent values as entries of that axis (for secondary lookup / pivoting).           |
| `@|`     | [`SquareColumnIs()`](https://tanaylab.github.io/dafr/reference/SquareColumnIs.md)     | Slice a square matrix by column.                                                             |
| `@-`     | [`SquareRowIs()`](https://tanaylab.github.io/dafr/reference/SquareRowIs.md)           | Slice a square matrix by row.                                                                |
| `/`      | [`GroupBy()`](https://tanaylab.github.io/dafr/reference/GroupBy.md)                   | Group a vector by values of another vector of the same length.                               |
| `|/`     | [`GroupColumnsBy()`](https://tanaylab.github.io/dafr/reference/GroupColumnsBy.md)     | Group matrix columns by a vector with one value per column.                                  |
| `-/`     | [`GroupRowsBy()`](https://tanaylab.github.io/dafr/reference/GroupRowsBy.md)           | Group matrix rows by a vector with one value per row.                                        |
| `%`      | eltwise op (`Abs`, …)                                                                 | Apply an element-wise operation to vector or matrix data.                                    |
| `>>`     | reduction op (`Sum`, …)                                                               | Reduce a vector or matrix to a scalar (per-group vector on a grouped input).                 |
| `>|`     | [`ReduceToColumn()`](https://tanaylab.github.io/dafr/reference/ReduceToColumn.md)     | Reduce a matrix along rows to a single column vector.                                        |
| `>-`     | [`ReduceToRow()`](https://tanaylab.github.io/dafr/reference/ReduceToRow.md)           | Reduce a matrix along columns to a single row vector.                                        |
| `\|\|`   | [`IfMissing()`](https://tanaylab.github.io/dafr/reference/IfMissing.md)               | Default value when a lookup target does not exist, or when reducing empty data.              |
| `??`     | [`IfNot()`](https://tanaylab.github.io/dafr/reference/IfNot.md)                       | Fallback value when a chained lookup produces an empty result.                               |
| `*`      | [`CountBy()`](https://tanaylab.github.io/dafr/reference/CountBy.md)                   | Count, in a matrix, the number of times each combination of values from two vectors appears. |
| `?`      | [`Names()`](https://tanaylab.github.io/dafr/reference/Names.md)                       | Return the set of axis / property names reachable from the current stack state.              |
| `.`      | [`LookupScalar()`](https://tanaylab.github.io/dafr/reference/LookupScalar.md)         | Look up a scalar property.                                                                   |
| `:`      | [`LookupVector()`](https://tanaylab.github.io/dafr/reference/LookupVector.md)         | Look up a vector property on the current axis.                                               |
| `::`     | [`LookupMatrix()`](https://tanaylab.github.io/dafr/reference/LookupMatrix.md)         | Look up a matrix property on a pair of axes.                                                 |
| `<`      | [`IsLess()`](https://tanaylab.github.io/dafr/reference/IsLess.md)                     | Mask comparison: strictly less.                                                              |
| `<=`     | [`IsLessEqual()`](https://tanaylab.github.io/dafr/reference/IsLessEqual.md)           | Mask comparison: less or equal.                                                              |
| `=`      | [`IsEqual()`](https://tanaylab.github.io/dafr/reference/IsEqual.md)                   | Mask comparison: equal.                                                                      |
| `!=`     | [`IsNotEqual()`](https://tanaylab.github.io/dafr/reference/IsNotEqual.md)             | Mask comparison: not equal.                                                                  |
| `>=`     | [`IsGreaterEqual()`](https://tanaylab.github.io/dafr/reference/IsGreaterEqual.md)     | Mask comparison: greater or equal.                                                           |
| `>`      | [`IsGreater()`](https://tanaylab.github.io/dafr/reference/IsGreater.md)               | Mask comparison: strictly greater.                                                           |
| `~`      | [`IsMatch()`](https://tanaylab.github.io/dafr/reference/IsMatch.md)                   | Mask comparison: regex match.                                                                |
| `!~`     | [`IsNotMatch()`](https://tanaylab.github.io/dafr/reference/IsNotMatch.md)             | Mask comparison: regex non-match.                                                            |
| `[`      | [`BeginMask()`](https://tanaylab.github.io/dafr/reference/BeginMask.md)               | Start a mask sub-expression on the current axis.                                             |
| `[ !`    | [`BeginNegatedMask()`](https://tanaylab.github.io/dafr/reference/BeginNegatedMask.md) | Start a negated mask on the current axis.                                                    |
| `]`      | [`EndMask()`](https://tanaylab.github.io/dafr/reference/EndMask.md)                   | Finish the mask; the axis becomes filtered.                                                  |
| `&`      | [`AndMask()`](https://tanaylab.github.io/dafr/reference/AndMask.md)                   | Combine masks with AND.                                                                      |
| `& !`    | [`AndNegatedMask()`](https://tanaylab.github.io/dafr/reference/AndNegatedMask.md)     | Combine masks with AND NOT.                                                                  |
| `\|`     | [`OrMask()`](https://tanaylab.github.io/dafr/reference/OrMask.md)                     | Combine masks with OR.                                                                       |
| `\| !`   | [`OrNegatedMask()`](https://tanaylab.github.io/dafr/reference/OrNegatedMask.md)       | Combine masks with OR NOT.                                                                   |
| `^`      | [`XorMask()`](https://tanaylab.github.io/dafr/reference/XorMask.md)                   | Combine masks with XOR.                                                                      |
| `^ !`    | [`XorNegatedMask()`](https://tanaylab.github.io/dafr/reference/XorNegatedMask.md)     | Combine masks with XOR NOT.                                                                  |

Element-wise operation builders include `Abs`, `Clamp`, `Convert`,
`Fraction`, `Log`, `Round`, `Significant`. Reduction operation builders
include `Sum`, `Mean`, `Median`, `Mode`, `Quantile`, `Max`, `Min`,
`Count`, `GeoMean`, `Std`, `StdN`, `Var`, `VarN`.

## The four query kinds

Every query returns one of four result kinds, selected by the phrases it
uses.

### Names query — `?`

Returns the set of names at some point in the data hierarchy.

| Goal                                | String                 | Builder                                             |
|-------------------------------------|------------------------|-----------------------------------------------------|
| Names of all scalars                | `". ?"`                | `LookupScalar("?")`                                 |
| Names of all axes                   | `"@ ?"`                | `Axis("?")`                                         |
| Vector properties on `gene`         | `"@ gene : ?"`         | `Axis("gene") |> LookupVector("?")`                 |
| Matrix properties on `(cell, gene)` | `"@ cell @ gene :: ?"` | `Axis("cell") |> Axis("gene") |> LookupMatrix("?")` |

``` r
cells[". ?"]
#> [1] "organism"  "reference"
cells["@ gene : ?"]
#> [1] "is_lateral"
cells["@ cell @ gene :: ?"]
#> [1] "UMIs"
```

### Scalar query — `.` / `... >> Reduction`

Returns a single scalar value. Produced by:

- Looking up a scalar (`. name`), optionally with a `||` default and an
  optional `type T` to pin the default’s dtype.
- Looking up a vector and picking one entry (`: vec @ axis = entry`).
- Looking up a matrix and picking one cell
  (`:: m @ rows = R @ cols = C`).
- Reducing any vector to a scalar (`... >> Reduction`).
- Reducing any matrix to a scalar (`... >> Reduction`).

``` r
cells[". organism"]
#> [1] "human"
cells[". nope || fallback"]
#> [1] "fallback"
cells[". nope || 0 type Int64"]
#> integer64
#> [1] 0

# Pick one entry from a vector.
cells["@ donor : age @ donor = N89"]
#> [1] 55

# Number of genes marked as lateral (reduce a vector of Bool).
cells["@ gene : is_lateral >> Sum type Int64"]
#> [1] 438

# Total UMIs in the whole matrix.
cells["@ cell @ gene :: UMIs >> Sum type Int64"]
#> [1] 1171936
```

### Vector query

Returns a vector. Phrase kinds:

- Axis entries: `@ axis`.
- Axis entries after masking: `@ axis [ ... ]`.
- Property lookup along an axis: `@ axis : property`.
- Any of the above with an element-wise op appended:
  `@ axis : property % Op`.
- Reduction of a matrix along one dimension: `@ rows @ cols :: m >- Op`
  (row result) or `>| Op` (column result).

``` r
head(cells["@ experiment"])
#> [1] "demux_01_02_21_1" "demux_01_02_21_2" "demux_01_03_21_1" "demux_04_01_21_1"
#> [5] "demux_04_01_21_2" "demux_07_03_21_1"
length(cells["@ gene [ ! is_lateral ]"])
#> [1] 245
head(cells["@ cell : donor"])
#> demux_07_12_20_1_AACAAGATCCATTTCA-1 demux_07_12_20_1_AACGAAAGTCCAATCA-1 
#>                               "N89"                               "N84" 
#> demux_07_12_20_1_AAGACAAAGTTCCGTA-1 demux_07_12_20_1_AGACTCATCTATTGTC-1 
#>                               "N86"                               "N84" 
#> demux_07_12_20_1_AGATAGACATTCCTCG-1 demux_07_12_20_1_ATCGTAGTCCAGTGCG-1 
#>                               "N89"                               "N89"
# Per-row (per-gene) mean UMIs.
head(cells["@ gene @ cell :: UMIs >- Mean"])
#> demux_07_12_20_1_AACAAGATCCATTTCA-1 demux_07_12_20_1_AACGAAAGTCCAATCA-1 
#>                            3.351391                            4.535871 
#> demux_07_12_20_1_AAGACAAAGTTCCGTA-1 demux_07_12_20_1_AGACTCATCTATTGTC-1 
#>                            2.411420                            4.131772 
#> demux_07_12_20_1_AGATAGACATTCCTCG-1 demux_07_12_20_1_ATCGTAGTCCAGTGCG-1 
#>                            2.641288                            3.929722
```

### Matrix query

Returns a matrix. Phrase kinds:

- Lookup: `@ rows @ cols :: property`.
- Count of value-combinations: `@ axis : v1 * v2`.
- Any of the above followed by an element-wise op, or a row / column
  grouping + reduction.

``` r
dim(cells["@ cell @ gene :: UMIs"])
#> [1] 856 683

# Log-transform a dense matrix.
dim(cells["@ cell @ gene :: UMIs % Log base 2 eps 1"])
#> [1] 856 683

# Group matrix rows by donor, then reduce each group to its mean.
dim(cells["@ cell @ gene :: UMIs -/ donor >- Mean"])
#> [1]  95 683
```

## Masks

A mask filters an axis to the entries for which a predicate holds.

Phrase structure: `@ axis [ <mask-body> ]`.

The mask body starts with a property lookup (`BeginMask`) and is
followed by zero or more **comparisons** and **mask combinators**.

``` r
# Donors older than 30.
length(cells[Axis("donor") |>
             BeginMask("age") |> IsGreater(30) |> EndMask()])
#> [1] 93

# Same in string form.
length(cells["@ donor [ age > 30 ]"])
#> [1] 93

# Negated mask: genes that are NOT lateral.
length(cells["@ gene [ ! is_lateral ]"])
#> [1] 245

# Combined masks: age > 60 AND sex = male (on donor axis).
length(cells["@ donor [ age > 60 & sex = male ]"])
#> [1] 29
```

Supported comparisons: `<`, `<=`, `=`, `!=`, `>=`, `>`, `~` (regex
match), `!~` (regex non-match).

Mask combinators: `&`, `& !`, `|`, `| !`, `^`, `^ !`. They are evaluated
left to right — they are **not** grouped by precedence. Use separate
masks with explicit combinators when order matters.

## Defaults and fallbacks

Two operators supply fallback values:

- `|| value` (`IfMissing`) — used when the thing being looked up does
  not exist, or when reducing an empty vector / matrix.
- `?? value` (`IfNot`) — used during chained lookups (`: a =@ : b`) when
  the intermediate key is empty.

``` r
cells[". nope || fallback"]
#> [1] "fallback"

# Fallback when a chained lookup runs out of pivot values.
head(metacells["@ cell : metacell =@ ?? none : type"])
#> demux_07_12_20_1_AACAAGATCCATTTCA-1 demux_07_12_20_1_AACGAAAGTCCAATCA-1 
#>                          "MEBEMP-E"                              "none" 
#> demux_07_12_20_1_AAGACAAAGTTCCGTA-1 demux_07_12_20_1_AGACTCATCTATTGTC-1 
#>                          "MEBEMP-E"                               "MPP" 
#> demux_07_12_20_1_AGATAGACATTCCTCG-1 demux_07_12_20_1_ATCGTAGTCCAGTGCG-1 
#>                          "MEBEMP-E"                              "none"
```

## Element-wise and reduction operations

Element-wise ops are applied with `%`; reductions on a matrix with `>-`
(reduce to row) or `>|` (reduce to column). Both take keyword-style
sub-arguments separated by spaces:

``` r
daf["... % Log base 2 eps 1e-5"]
daf["... >- Mean"]
daf["... >| Sum type Int64"]
```

Built-in element-wise: `Abs`, `Clamp low h`, `Convert type T`,
`Fraction`, `Log base b eps e`, `Round`, `Significant digits n`.

Built-in reductions: `Sum`, `Mean`, `Median`, `Mode`, `Quantile p P`,
`Max`, `Min`, `Count`, `GeoMean`, `Std`, `StdN`, `Var`, `VarN`.

User code can extend either set:

``` r
register_eltwise("MySquare", function(x) x * x)
register_reduction("MyMax", function(x) max(x, na.rm = TRUE))
```

## Grouping

Grouping appears in three phrase contexts:

- `GroupBy` (`/`) — reduce a vector to groups defined by another vector
  of equal length, yielding a shorter vector keyed by group.
- `GroupRowsBy` (`-/`) — group the rows of a matrix and emit one row per
  group.
- `GroupColumnsBy` (`|/`) — group the columns of a matrix and emit one
  column per group.

The matrix-group forms must be paired with a compatible reduction (`-/`
pairs with `>-`, `|/` with `>|`).

``` r
# Mean per (donor, gene) — collapse cells into donor rows.
dim(cells["@ cell @ gene :: UMIs -/ donor >- Mean"])
#> [1]  95 683
```

Appending `=@` after a group property changes the result to be ordered
by that axis’s entries (rather than by unique group value). Appending
`|| default` before the reduction supplies a value for empty groups.

## Axis-as-value and chained lookup (`=@`)

`=@` declares that a set of values should be interpreted as entries of a
named axis. That lets the query walk one axis into another:

``` r
# Cell -> metacell -> type: pipe values through the metacell axis.
head(metacells["@ cell : metacell =@ ?? none : type"])
#> demux_07_12_20_1_AACAAGATCCATTTCA-1 demux_07_12_20_1_AACGAAAGTCCAATCA-1 
#>                          "MEBEMP-E"                              "none" 
#> demux_07_12_20_1_AAGACAAAGTTCCGTA-1 demux_07_12_20_1_AGACTCATCTATTGTC-1 
#>                          "MEBEMP-E"                               "MPP" 
#> demux_07_12_20_1_AGATAGACATTCCTCG-1 demux_07_12_20_1_ATCGTAGTCCAGTGCG-1 
#>                          "MEBEMP-E"                              "none"
```

In the above, `: metacell =@` says “these values are entries of the
`metacell` axis”, then `: type` looks up the `type` property on that
axis. The `?? NA` fallback covers cells without an assigned metacell.

`SquareColumnIs` (`@|`) and `SquareRowIs` (`@-`) slice a square matrix
by a specific column / row of that axis; the right-hand side of each is
a value of the matrix’s square axis.

## Parsing, canonicalisation, and equivalence

`parse_query(s)` returns the builder AST; `canonical_query(q)` returns a
normalised string; both forms of the same query produce the same AST.

``` r
s     <- "@ gene : is_lateral"
q_bld <- Axis("gene") |> LookupVector("is_lateral")
identical(canonical_query(s), as.character(q_bld))
#> [1] TRUE
```

Inspection helpers:

- `has_query(daf, q)` — `TRUE` if `q` could be evaluated against `daf`.
- `is_axis_query(q)` — `TRUE` if the result is an axis.
- `query_axis_name(q)` — the axis name (if applicable).
- `query_result_dimensions(q)` — 0 (scalar), 1 (vector), 2 (matrix).
- `query_requires_relayout(q)` — whether a matrix relayout is needed.

## String literals and escaping

String literal values inside a query can include spaces and most
punctuation. Characters that collide with the DSL’s own tokens (`@`,
`:`, `|`, `[`, `]`, …) must be escaped. Use
[`escape_value()`](https://tanaylab.github.io/dafr/reference/escape_value.md)
to produce a safe literal and
[`unescape_value()`](https://tanaylab.github.io/dafr/reference/unescape_value.md)
to reverse it.

``` r
escape_value("a | b")
#> [1] "\"a | b\""
```

## Caching

`get_query(daf, q)` caches the result as `QueryData` inside the daf’s
internal cache. Use `daf[q]` for a one-shot evaluation that does **not**
cache. `empty_cache(daf)` releases all cached results and is also
invoked automatically on every mutating write to the daf so cached
results never become stale.

## Further reading

- [Query DSL tutorial](https://tanaylab.github.io/dafr/queries.md) —
  short introduction.
- [Reference: Query DSL (string
  form)](https://tanaylab.github.io/dafr/reference/index.md) —
  per-function docs for parsing, inspection, and escape helpers.
- [Reference: Query
  builders](https://tanaylab.github.io/dafr/reference/index.md) —
  per-function docs for each builder.
- Upstream spec in
  [`DataAxesFormats.jl/src/queries.jl`](https://github.com/tanaylab/DataAxesFormats.jl/blob/main/src/queries.jl)
  for semantic cross-reference.
