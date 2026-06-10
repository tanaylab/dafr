# R query parity: adversarial findings

Methodology: 320 hand-crafted queries (`queries_all.txt`) evaluated through both
DataAxesFormats.jl (`parse_query` + `get_query`) and dafr (`get_query`) against
an identical fixture (`build_fixture.jl` → `fixture.daf`, FilesDaf backend so
both languages read the same on-disk bytes). Outputs serialized to JSONL and
diffed (`diff.py`).

Result: 294 queries surveyed (some lines join across continuations), 206 match,
36 both-error parity, 19 vector_diff, 5 kind_mismatch, 4 scalar_diff, 20
jl_err_r_ok, 4 jl_ok_r_err.

The categorisation below ranks divergences by impact.

---

## A. Silent wrong answers in R (top severity)

These are the most dangerous: R returns a value, the user has no signal that
anything is wrong, and the value disagrees with Julia.

### A1. Grouped reductions on integer64 vectors → memory garbage

When the in-scope vector is `integer64` (storage `double` typed, e.g. anything
Julia stored as `Int64`), the fast path at `R/query_eval.R:2471`
(`.grouped_vector_builtin`) calls `rowsum(x, group)` which treats the raw
double bytes as numbers - re-interpreting int64 bits as float64.

The check `if (!is.na(label) && is.numeric(x))` at line 2600 passes because
`typeof(integer64) == "double"`. Fix: detect `inherits(x, "integer64")` and
`as.double(x)` (preserving names) before the fast path.

Reproducer (fixture has `age = 10,20,30,40,50` stored as Int64,
`type = U,V,U,W,V`):

```r
> get_query(daf, "@ cell : age / type >> Sum")
            U             V             W
1.976263e-322 3.458460e-322 1.976263e-322
```

Affected: Sum, Mean, Min, Max, Var, Std, VarN, StdN, GeoMean.
`Count` works (no value bytes needed). `Mode` not tested on int64.

Same root cause:
- `@ cell : age / batch >> Sum` → zeros
- `@ cell : age / is_doublet >> Var` → `[0, 0]`
- `@ cell : age % Abs / type >> Sum` → zeros (the % Abs keeps int64)
- `@ cell : age % Convert type Float32 / type >> Sum` → zeros (Convert
  fast path also leaves int64 class - see A3)
- `@ cell : age / type =@ >> Sum [|| 0]` → zeros

### A2. `% Round type Int8` on integer64 → all zeros

```r
> get_query(daf, "@ cell : age % Round type Int8")
A B C D E
0 0 0 0 0
```

Julia: `[10, 20, 30, 40, 50]`. Same byte-reinterpret family of bugs as A1, but
in the eltwise Round kernel dispatch.

### A3. `% Convert type <T>` on integer64 leaves class `integer64` with garbage bytes

```r
> r <- get_query(daf, "@ cell : age % Convert type Float32")
> unclass(r)
            A             B             C             D             E
4.940656e-323 9.881313e-323 1.482197e-322 1.976263e-322 2.470328e-322
```

Class is still `integer64` (wrong - Convert should change class) and values are
junk doubles. `% Convert type integer` produces all zeros with `typeof =
integer` but `class = integer64` (a corrupt object).

### A4. String ordering uses R `LC_COLLATE`, not bytewise

Julia compares strings bytewise (`"Z"(0x5A) < "b"(0x62)` → true). R uses the
process LC_COLLATE which under default `en_US.UTF-8` returns
`"Z" < "b"` → false (uppercase Z folds near lowercase z, after b). R's compare
ops at `R/query_eval.R:1432` / `:1450` use bare `<` / `<=` / `>` / `>=`.

Reproducer (fixture labels: `["", "x", "y\\z", "a b", "Z"]`):

```r
> get_query(daf, "@ cell [ label < b ]")
  A   D       # Julia returns A, D, E - "Z" missing in R
> get_query(daf, "@ cell [ label > Z ]")
named character(0)   # Julia returns B, C, D
> get_query(daf, "@ cell [ label <= Z ]")
  A   B   C   D   E    # Julia returns A, E
```

Equality (`=` / `!=`) is bytewise in R, so it agrees. Only ordering diverges.

The fix is to compare in C-locale or use `bitwAnd`/`Encoding`-aware comparison.
Easiest: wrap comparisons in `withCallingHandlers` after
`Sys.setlocale("LC_COLLATE", "C")`, restored at exit. Or convert both sides to
raw bytes and compare lexicographically.

This is a **high-priority** bug because it silently changes which entries pass
through a mask whenever string properties contain mixed case or non-ASCII.

### A5. `>> Median` on a vector containing `NaN` returns `NA` instead of `NaN`

```r
> get_query(daf, "@ cell : with_nan >> Median")
[1] NA       # Julia: NaN
```

Cause: base R's `median()` treats `NaN` as `NA`. Other reductions correctly
return `NaN` (Mean, Sum, Max, Min match). The grouped Median path uses
`stats::quantile(v, q, type = 7L)` so it has the same issue (also see B3).

### A6. `>| GeoMean` precision diff

```
[vector_diff] @ cell @ gene :: UMIs >| GeoMean
    jl=[1.817121, 4.932424, 7.958115, 0.0, 3.914868]
    r =[1.817121, 4.932424, 7.958114, 0.0, 3.914868]
```

One element differs in the 7th significant digit, consistent with a Float32
intermediate in one path. Low impact but a real divergence on int32-input
matrices.

---

## B. R errors where Julia returns a value (functionality regression)

### B1. `|| pi <Type>` / `|| e <Type>` / `Log base e` - typed named constants

Julia recognises `pi` / `e` in `guess_typed_value()`
(queries.jl:1923-1941) even when an explicit type follows; R's
`.coerce_if_missing_default` doesn't:

```r
> get_query(daf, ". missing_scalar || pi Float32")
[1] NA   # Julia: 3.1415927
> get_query(daf, ". missing_scalar || e Float64")
[1] NA   # Julia: 2.718281828459045
> get_query(daf, "@ cell : age % Log base e eps 0.0")
Error: invalid value: "e" / value must be: a number / for parameter: base
```

`|| pi` (without type) works in R - because the un-typed branch hits a different
code path that does recognise `pi`. Bug is in the explicit-type coercion path.

### B2. `% Convert type UInt8 / UInt16 / UInt32 / UInt64 / Int8 / Int16`

R refuses every Julia int type that doesn't have a built-in R analogue:

```r
> get_query(daf, "@ cell : age % Convert type UInt8")
Error: Convert: 'type' must be one of 'double', 'integer', 'logical',
  'integer64' (or Julia aliases 'Float32'/'Float64'/'Int32'/'Int64'/'Bool');
  got 'UInt8'
```

Julia accepts these; for in-range values it converts cleanly, for out-of-range
it raises `InexactError`. R simply rejects the type name at parse, so any
project that emits these types from Julia tooling fails on R queries.

Same gap applies to `% Round type UInt32` etc.

### B3. Grouped Median on integer64 → "only type==0 ('qtile') supported"

```r
> get_query(daf, "@ cell : age / batch >> Median")
Error: only type==0 ('qtile') supported
```

Root cause: line 2508 calls `stats::quantile(v, q, type = 7L, names = FALSE)`.
The `bit64` package's S3 method for `quantile.integer64` only implements type 0
(`?bit64::qtile`). Same fix shape as A1 - cast integer64 to double before
calling `stats::quantile`.

### B4. `score % Round type UInt32` succeeds on negative float

```r
> get_query(daf, "@ cell : score % Round type UInt32")   # score has -1.0
# Returns silently (with wraparound or coerce)
```

Julia: `InexactError: UInt32(-1.0)`. R silently accepts.

---

## C. R lenient where Julia rejects (parser strictness)

Per project policy (Julia is authoritative), these are bugs even though R is
"accepting more". Each requires the R parser/coercer to validate the same as
Julia.

### C1. `% Convert/Abs/Round type <Int>` silently coerces `.5` values

```r
> get_query(daf, "@ cell : score % Abs type UInt8")    # 0.5, 1.5, 2.5, 1.0, 3.5
# Returns a vector (rounds or truncates silently)
```

Julia: `InexactError: UInt8(0.5)`. R should raise the same error at the kernel.

### C2. `?? <bare value>` (IfNot)

Julia rejects `?? a` as invalid syntax (IfNot takes a chained lookup, not a
bare value), R accepts and treats the bare value as the default. Same for
`?? unknown`.

```r
> get_query(daf, "@ cell : type ?? a")
# returns: type vector with "a" wherever type is missing
```

This is a real Julia syntax error per Julia's tokeniser (the `??` operator
expects an operation, not a value).

### C3. Non-ASCII byte in unquoted IfMissing default

```r
> get_query(daf, ". missing_scalar || héllo")
# R: accepts, returns "héllo"
# Julia: unexpected character: 'é'
```

The Julia tokens.jl regex permits non-ASCII Unicode in *value* tokens but its
operator-precedence pass before value-token resolution rejects it as an
"unexpected character". R is more permissive. Worth aligning regardless of
which side is "right" - they should match.

### C4. `|| 1.5 Int32` is rejected at parse time in Julia even when default is unused

```r
> get_query(daf, ". intver || 1.5 Int32")
[1] 7   # intver exists, IfMissing unused
```

Julia rejects this at parse: "invalid value: '1.5' / value must be: a valid
Int32". R only validates the default when needed. Same case as known gap
P-class but worth confirming whether this is intentional (deferred) or new.

---

## D. Names-query API shape (Set vs character vector)

```
. ? → Julia: Set{AbstractString}     | R: character()
@ ? → Julia: Set{AbstractString}     | R: character()
@ cell : ? → same                    | R: character()
@ cell @ gene :: ? → same            | R: character()
@ empty_axis : ? → same              | R: character()
```

The actual element values match - only the R wrapper differs. If the policy is
"unordered set of names", R should return a `setNames(character(0)/(n),
NULL)` with no implicit alphabetical ordering, or document the divergence.

---

## E. Known/deferred gaps (already documented in dev/notes)

Re-confirmed by this harness, included for completeness:

- E5: Top-level `: vec @ axis = entry` and matrix entry-pick variants
  - `@ cell : score @ cell = A` (R extends, Julia rejects)
  - `@ cell @ gene :: UMIs @ gene = g1` (R extends, Julia rejects)
  - `@ cell @ cell :: distance @- A` (R extends, Julia rejects)
- E10: Regex with escaped brackets
  - `@ cell [ label ~ ^\[a-z\] ]` (R extends, Julia rejects)
- Similar: `@ cell [ type ~ ^U ]` - Julia rejects unescaped `^` (it's an
  operator), R accepts as regex. Worth recording alongside E10.

---

## Bug-fix order suggestion (impact × ease)

1. **A4 (locale comparison)** - one-line wrap, affects every mixed-case mask.
2. **A1 (grouped reduction on int64)** - `as.double(x)` (preserve names) at the
   start of `.grouped_vector_builtin` and `.grouped_matrix_builtin`.
3. **B3 (grouped Median int64)** - same fix as A1 covers it.
4. **A2 / A3 (Round / Convert int64 eltwise)** - same cast pattern in the
   eltwise dispatch.
5. **B1 (named constants with typed `||`)** - extend
   `.coerce_if_missing_default` to recognise `pi` / `e` (and any others Julia
   recognises) before falling through to `as.<T>(value_str)`.
6. **B2 (UInt/Int sizes in Convert/Round)** - extend the type allow-list. R
   integer storage is 32-bit, but we have integer64 and double for big ints; a
   range check on conversion result gives parity with Julia's `InexactError`.
7. **A5 (Median on NaN)** - special-case NaN in the Median fallback (the
   builtin path uses `stats::quantile` which returns NA; switch to a manual
   sort+midpoint that propagates NaN, or pre-check `any(is.nan(x))`).
8. **C1, C2, C3, C4 (parser strictness)** - smaller individual fixes, lower
   user-visible impact than A-class.
9. **D (names-query type)** - decide policy.

---

## Round 4 - additional adversarial probes (2026-05-15)

Fixture extended with `all_nan`, `infs`, `all_neg`, `f32_score`, `i8_age`,
`i16_age`, `u32_count`, `ties_int`, `subtype`, and a sparse-friendly
`sparse_umis` matrix. 134 new queries in `queries4.txt` targeting unexplored
corners: NaN propagation, Inf/-Inf inputs, IfMissing with overflow literals,
overflow-typed reductions, lowercase type aliases, log of negatives, string
Min/Max, `=@` with non-axis property, Mode tie-breaking, etc.

Result: 240 queries surveyed, **all but one** now agree with Julia. The lone
divergence is an edge-case overflow handling difference for `1e309`.

### Fixed in this round (now match Julia)

All previously divergent. Now mitigated:

**Var / Std / VarN / StdN on NaN input** (`R/operations.R:.var_uncorrected`)

R was using `anyNA(x) -> NA_real_`, masking computational NaN with R's
algebraic NA. Now: NaN-in-input → NaN-out; only real NA (which Julia never
produces) yields NA.

**IfMissing default replacing computation-NaN** (`R/query_eval.R:finalize_vals`)

For `... / type =@ >> Sum || 0.0`, when a group contained NaN data the per-group
sum is NaN. R's finalizer was replacing that NaN with the IfMissing default.
Julia propagates NaN: `|| <default>` only fills entries that don't exist in
the target axis at all (no group members). Now: finalizer tracks
`missing_idx` (the indices added by the axis expansion) and only fills those.

**IfMissing default NaN / Inf / Infinity literals** (`R/query_eval.R:.resolve_if_missing_constant`)

Julia's `guess_typed_value` delegates to `parse(Float64, value)`, which
accepts `nan`, `inf`, `Infinity`, `-Inf`, etc. case-insensitively and returns
the IEEE special. R's coercer now recognises them too, both with and without
an explicit `Float32` / `Float64` type tag.

**Lowercase Julia type aliases** (`R/operations.R:.canonicalize_julia_type`,
`.cast_to_type`, `.reject_non_float_type`, `.reject_non_number_type`,
`.op_convert`)

Julia's `DTYPE_BY_NAME` map (`operations.jl:216-242`) accepts both `Int8` and
`int8`, `Float64` and `float64`, etc. R now canonicalises lowercase aliases
to their cased equivalent before downstream range / inexact checks fire, so
`% Convert type float64` works and `% Round type int8` validates correctly.

**`% Fraction` on NaN vectors** (`R/operations.R:.op_fraction`)

`sum(x)` on `with_nan` is NaN; the old `if (total == 0)` branch hit "missing
value where TRUE/FALSE needed" because R errors on `NaN == 0` (yields NA, not
a logical). Now: skip the zero-special-case when `total` is NA/NaN and let
the division propagate NaN like Julia.

**`% Log` rejection of negative values, accept zero** (`R/operations.R:.op_log`,
`R/query_eval.R:.apply_eltwise` fast path)

R was rejecting `x + eps <= 0` (including zero), which the Julia DAF allows
(`log(0) = -Inf`). Julia raises `DomainError` only for *negative* arguments.
Now: R uses `< 0`, matches Julia's error message
(`DomainError with X: log was called with a negative real argument`), and the
fast-path C++ kernel respects the same gate (it was silently producing
NaN for negative input).

**`>> GeoMean` rejection of negative values** (`R/operations.R:.op_geomean`,
`R/query_eval.R:.grouped_vector_builtin`)

Same as Log. Both the slow path and the grouped fast path now raise
DomainError on negative input instead of returning silent NaN.

**String `>> Min` / `>> Max` rejection** (`R/query_eval.R:.apply_reduction_to_scalar`,
`.apply_reduction_grouped_vector`)

Julia explicitly rejects string input on reductions other than Mode/Count
(`unsupported input type: String for the reduction operation: Min`). R was
running base R `min()` / `max()` (alphabetical order). Now mirrors Julia.

**Reduction `type` parameter validation** (`R/operations.R:.op_sum`,
`.op_mean`, `.op_count`, `.op_var`, `.op_std`, `.op_varn`, `.op_stdn`,
`.op_median`, `.op_quantile`)

Julia accepts only float types for Mean / Median / Var / Std / VarN / StdN /
Quantile (via `parse_float_type_value`), and any number type for Sum / Count
(via `parse_number_type_value`). Result is also cast / range-checked, so
`>> Sum type Int8` on a sum of 150 raises `InexactError: Int8(150)`, and
`>> Mean type Int8` raises `invalid value: "Int8" / value must be: a float type`.
R was silently ignoring `type` for these ops.

**`=@` requires an actual axis** (`R/query_eval.R:.apply_reduction_grouped_vector`)

For `... / prop =@ >> Op`, bare `=@` derives the broadcast target from the
grouping property's source axis. If `prop` isn't an axis (e.g.
`/ subtype =@` where `subtype` is a cell property whose values are `U` / `V`),
Julia raises `missing axis: subtype`. R was silently treating `=@` as a no-op
when the target wasn't an axis. Now mirrors Julia.

### Still divergent (intentional / edge-case)

`. missing_scalar || 1e309` - Julia keeps the literal as a `String`,
R parses it to `Inf` via Float64 overflow. Both are defensible
interpretations; the difference is one specific overflow case at the
IfMissing parsing boundary. Not worth chasing.

---

## Round 5 - new adversarial probes (2026-05-15)

200+ new queries in `queries5.txt` targeting unexplored corners:
type-coerced mask comparisons (int prop vs float literal, bool prop vs
0/1 literal), NaN/Inf in comparison RHS, mask & CountBy combinations,
chained `% Convert`, `>> Op type T` on Min/Max/Mode/Quantile, group by
float prop, sparse-matrix groupby, IfMissing with overflow / hex / large
ints, regex of bare `.`/`.*`, chained `=@`, names query on filtered
axis. 255 queries surveyed → **112 match, 95 both-error, 48 divergent**.

The divergences cluster into clear bug families. R/operations.R and
R/query_eval.R locations are given so each is mechanically actionable.

### A. Silent wrong-answer in R (top severity)

#### A7. Mask `int-prop = <float-literal>` matches the truncated integer

`age` is Int64 on disk → bit64::integer64 in R. `.coerce_cmp` at
`R/query_eval.R:1645` returns `as.numeric("20.5") = 20.5`, but
`integer64 == 20.5` silently truncates the RHS to 20:

```r
> get_query(daf, "@ cell [ age = 20.5 ]")
B
"B"                  # Julia: empty (no entry equals 20.5)

> get_query(daf, "@ cell [ age < 20.5 ]")
A
"A"                  # Julia: [A, B]      ← bit64 truncates < 20.5 to < 20

> get_query(daf, "@ cell [ age != 20.5 ]")
A C D E              # Julia: [A, B, C, D, E]  ← bit64 truncates != 20.5 to != 20
```

This is the most dangerous Round-5 finding: a mask query silently
returns the wrong rows whenever the comparison constant has a fractional
part and the property is Int64. Fix: in `.validate_comparator`, when the
ref vec inherits `integer64` and the parsed numeric isn't integer-valued,
either reject (Julia errors via `parse_int_value` on the comparator) or
convert the vector to double before comparing. The Julia code path uses
`parse_number_comparison_value` with the column's eltype so a non-Int
literal can't compare equal to any Int cell.

#### A8. Mask `bool-prop = 1` / `bool-prop = 0` silently drops every cell

```r
> get_query(daf, "@ cell [ is_doublet = 1 ]")
named character(0)   # Julia: [A, C, E]

> get_query(daf, "@ cell [ is_doublet = 0 ]")
named character(0)   # Julia: [B, D]
```

`.coerce_cmp(value_string, ref_vec)` at `R/query_eval.R:1645-1653` calls
`as.logical("1")`, which returns `NA` (R's `as.logical` only recognises
`"TRUE"`/`"FALSE"`/`"T"`/`"F"`). Julia accepts `0`/`1` as bool literals
in `parse_bool_value` (queries.jl). Fix: handle `"0"`/`"1"` (and the
strings `"true"`/`"false"`) explicitly before `as.logical`.

Worth noting: `is_doublet = true` errors in both languages (Julia rejects
unquoted `true` as a comparator value, R also rejects). The asymmetry is
that Julia *does* accept `0`/`1`.

#### A9. `% Convert type Float32` on String input returns a vector of NA

```r
> get_query(daf, "@ cell : type % Convert type Float32")
  A   B   C   D   E
 NA  NA  NA  NA  NA   # Julia: errors "unsupported input type: String"
```

`.op_convert` accepts the type token, then `as.<T>(character_vec)`
returns NA with a coercion warning. Julia explicitly rejects non-numeric
input at the eltwise dispatch. Fix: add an input-type guard at
`R/operations.R:.op_convert` (and any other numeric eltwise that the
string path can reach - `% Abs`, `% Round` already error parity-wise via
storage mode, but `% Convert` doesn't).

#### A10. Group-by `with_nan` silently buckets NaN-valued cells

```r
> get_query(daf, "@ cell : age / with_nan >> Sum")
 1  3  5 NA
10 30 50 60     # Julia: errors "no IfMissing value specified for the
                #                unused entry: NaN of the axis: nothing"
```

The R fast path treats NaN as just another distinct group label
("NA"-named bucket). Julia treats NaN-as-group-label as an "unused entry"
and demands an IfMissing default. Fix: in
`.grouped_vector_builtin` (or the upstream grouping path at
`R/query_eval.R`), detect NaN/NA in the grouping vector and either raise
the same error or honor IfMissing.

#### A11. CountBy after a mask raises "all arguments must have the same length"

```r
> get_query(daf, "@ cell [ is_doublet ] : type * batch")
Error: all arguments must have the same length      # Julia returns the
                                                    # 2x2 cross-tab
```

`.apply_countby` at `R/query_eval.R:3701-3733`:

- `a <- state$value`  → masked vector (length = mask-true count)
- `b <- format_get_vector(daf, state$axis, node$property)$value`
  → full property vector (length = full axis)

Then `table(a, b)` errors on length mismatch. Fix: when `state$indices`
(the mask-preserved index set) is present, subset `b` with it before
materialising the cross-tab. Same fix probably required in
`.apply_chained_lookup_count` for masked `* prop : prop2` chains.

### B. R errors where Julia returns a value (functional regression)

#### B5. `with_nan = NaN` / `!= NaN` in masks

```r
> get_query(daf, "@ cell [ with_nan = NaN ]")
Error: error parsing number comparison value: NaN ...
```

`.validate_comparator` at `R/query_eval.R:1687` does
`suppressWarnings(as.numeric("NaN"))` which returns `NaN` (so the
`is.na(n)` test fires for NaN too, since `is.na(NaN) == TRUE`). Julia
permits the comparison (and `NaN == NaN` is false, so it returns empty).
Fix: distinguish `NaN`/`Inf`/`-Inf` from "unparseable" in the validator,
or compare `is.nan(n)` separately and accept it.

#### B6. `% Log base NaN` / `% Log base 2 eps NaN` reject the parameter

```r
> get_query(daf, "@ cell : age % Log base NaN")
Error: invalid value: "NaN" / value must be: a number / for the parameter: base
```

Julia's `parse_float_value` accepts `NaN` (via `parse(Float64, "NaN")`)
and propagates it through the eltwise (returning a NaN vector). R's
eltwise param coercion rejects NaN as "not a number". Fix: the param
coercer used by `% Log`, `% Significant`, `% Clamp` etc. should
recognise NaN/Inf the same way `.resolve_if_missing_constant` was fixed
in Round 4.

#### B7. `% Significant` with NaN inputs or NaN/Inf bounds

```r
> get_query(daf, "@ cell : with_nan % Significant high 1")
Error: missing value where TRUE/FALSE needed

> get_query(daf, "@ cell : score % Significant high NaN")
Error: invalid value: "NaN" / value must be: positive / for the parameter: high
```

Same `% Log`-family pattern. The R Significant kernel does
`if (abs(x) < high)` which errors when `x` contains NaN (the comparison
returns NA, and the `if` branch sees NA). Julia's Significant gracefully
returns NaN for NaN input. Fix: vectorise the threshold check (use
`ifelse` / direct `pmin`-style), and stop rejecting NaN as a parameter
value.

#### B8. Matrix `>> Sum type Int32` on Float32-source raises InexactError

```r
> get_query(daf, "@ cell @ gene :: frac >> Sum type Int32")
Error: InexactError: Int32(6)        # Julia returns 6 (Int32)
```

The Float32 sum of the `frac` matrix is `6.0000004...` in R (one extra
Float32 rounding step), which fails the strict `value == round(value)`
check in `.strict_int_coerce`. Julia accumulates in Float64 and casts a
clean 6.0. Fix: when the source is Float32 and the cast target is an
integer type, accumulate in Float64 (use `as.double()` before `sum()`)
to drop the Float32 rounding noise. Or relax the InexactError check by a
single-ulp tolerance.

#### B9. `@ cell : type =@ : color =@` (chained `=@` on a lookup chain)

```r
> get_query(daf, "@ cell : type =@ : color =@")
Error: invalid query: @ cell : type =@ : color =@
```

Julia returns `["red", "green", "red", "blue", "green"]` (each cell's
type's color, broadcast back to cells). R's parser refuses the second
`=@`. Same family for `@ cell : batch =@ : donor =@`. Fix: extend the
parser/applier in `query_eval.R` to accept `=@` at the end of a chained
lookup, not just the first hop.

#### B10. `@ cell @ cell :: distance @- A` / `@| C` rejected by Julia, accepted by R

```r
> get_query(daf, "@ cell @ cell :: distance @- A")    # R: vector
                                                       # Julia: invalid operation(s)
```

E5-class extension: R's top-level matrix-axis row/column-pick is too
permissive on square-cell-cell matrices. Documented as known divergence
in queries.txt; reconfirmed.

### C. R lenient where Julia rejects (parser strictness)

These all follow the same shape: R accepts a query Julia rejects. Per
project policy, R is the one to tighten.

#### C5. `>> Min type X`, `>> Max type X`, `>> Mode type X` accept a `type` param Julia doesn't

```r
> get_query(daf, "@ cell : age >> Min type Int8")
[1] 10           # Julia: "the parameter: type does not exist for the operation: Min"
```

Min, Max, Mode do not take a `type` parameter in Julia. R silently
ignores the unknown param (likely a `...` swallow in `.op_min` / `.op_max` /
`.op_mode`). Fix: tighten the param signatures to error on unknown args,
matching `.reject_non_float_type` shape from Round 4.

#### C6. Matrix-reduction `>- Mean type Int32` not validated

```r
> get_query(daf, "@ cell @ gene :: UMIs >- Mean type Int32")
                                  # Julia: "type must be a float type"
```

The Round-4 fix added `.reject_non_float_type` to `.op_mean` for the
vector path, but the matrix-reduction (`>-` / `>|`) dispatch hits a
different code path that bypasses it. Fix: route matrix reductions
through the same param-validating front door.

#### C7. `is_doublet % Significant high 0.5` accepted on a Bool prop

```r
> get_query(daf, "@ cell : is_doublet % Significant high 0.5")
                                  # Julia: InexactError: Bool(0.5)
```

Julia's Significant uses the *input element type* for the threshold —
`0.5` is fractional and Bool can't hold it, so InexactError fires
inside Significant's parse. R uses double internally throughout and
doesn't care. Fix: when input is logical, validate that numeric
parameters round-trip to the storage type.

#### C8. IfMissing default range/inexact-check skipped on large/negative integers

```r
. missing_scalar || 4294967296          → R: NA       | Julia: Int64(4294967296)
. missing_scalar || 4294967296 Int32    → R: NA       | Julia: errors (out of Int32)
. missing_scalar || -1 UInt32           → R: -1       | Julia: errors
. missing_scalar || -1 UInt8            → R: -1       | Julia: errors
. missing_scalar || 256 UInt8           → R: 256      | Julia: errors
. missing_scalar || 256 Int8            → R: 256      | Julia: errors
. missing_scalar || 128 Int8            → R: 128      | Julia: errors
```

Two separate bugs:
1. **`4294967296` without type tag** - Julia returns Int64 (since 32-bit
   doesn't fit); R returns NA because integer-promotion to integer64
   isn't applied to bare-literal IfMissing values.
2. **Out-of-range typed defaults** - the cased-type alias canonicaliser
   added in Round 4 doesn't run the InexactError range-check when the
   default is *unused*. Julia checks all default literals at parse time
   regardless of whether they're consumed (mirrors C4 in Round 3 for
   `|| 1.5 Int32`). Fix: extend `.resolve_if_missing_constant` to
   validate the parsed literal against the declared type's range
   independent of consumption.

#### C9. `@ cell [ filter ] : ?` (names query on filtered axis)

```r
> get_query(daf, "@ cell [ is_doublet ] : ?")
[1] "age" "batch" ...                   # Julia: "invalid operation(s)"
```

Same for `@ cell @ gene [ is_lateral ] :: ?`. Julia requires `?` to be
applied to a bare axis spec, not a mask-narrowed one. R accepts. Fix:
reject `?` after a `[ mask ]` in the names-query branch.

#### C10. Regex `~ .` / `~ .*` accepted by R, rejected by Julia

```r
> get_query(daf, "@ cell [ label ~ . ]")
                                  # Julia: "expected: value"
```

Julia tokeniser treats bare `.`/`.*` as an empty-value error. R passes
them through to `grepl`, where `.` is the wildcard regex. Worth aligning
because users may write `~ .` expecting a "match any" predicate.

### D. Low-severity / precision / cosmetic

#### D1. Group-by names: `-1.0` (Julia) vs `-1` (R) for Float labels

```r
> get_query(daf, "@ cell : age / score >> Sum")
# names: jl=['-1.0','0.5','1.5','2.5','3.5']  r=['-1','0.5','1.5','2.5','3.5']
```

R's `format(-1)` drops the `.0` when stringifying a whole-numbered
Float64. Julia keeps it via `string(-1.0)`. Fix in the group-label
stringifier in `.finalize_grouped_vector`.

#### D2. `>- Mean % Log` 7th-digit precision drift

```
@ cell @ gene :: UMIs >- Mean % Log base 2 eps 1
    jl=[2.0, 2.321928, 2.584962]
    r =[2.0, 2.321928, 2.584963]   # 1 ulp at element 3
```

Same family as A6 in queries.txt (Float32 intermediate vs Float64).

#### D3. Hex/binary numeric literals in IfMissing

```
. missing_scalar || 0x10   → R: "0x10" (string)   | Julia: 16
. missing_scalar || 0b10   → R: "0b10" (string)   | Julia: 2
```

Julia's `parse(Int, ...)` recognises `0x` and `0b` prefixes. R's
`as.integer` doesn't. Fix in `.resolve_if_missing_constant` to detect
the prefixes and route through `strtoi(value, base = 16/2)`.

#### D4. `-1e309` overflow

Mirror of the Round-4 `+1e309` finding. Julia keeps the token as String;
R parses to -Inf via Float64 overflow. Same recommendation: not worth
chasing.

### Fixed in this round (now match Julia)

After the implementation pass, 42 of the 48 Round-5 divergences are
resolved. Test suite goes from 1 fail → 1 fail (the same pre-existing
`test-operations-registry.R:117`), no new regressions.

**A7** `int-prop = <float>` mask
(`R/query_eval.R:.promote_int64_for_fractional_cmp`):
when the in-scope vector is `integer64` and the parsed RHS has a
fractional part, promote the vector to `as.double` before comparing
so bit64's silent truncation can't fire. Applied at all three
comparator sites (axis-vector mask, plain vector, matrix, scalar).

**A8** `bool-prop = 0/1` mask (`R/query_eval.R:.coerce_cmp`):
when the ref vec is logical, recognise the exact tokens `"0"`,
`"1"`, `"true"`, `"false"` (lowercase only, matching Julia's
`parse(Bool, ...)` accept set). `"TRUE"` / `"True"` still error in
both languages (Julia's parser also rejects them).

**A9** `% Convert type Float32` on String input
(`R/operations.R:.op_convert`): reject character / factor input up
front with `unsupported input type: String for the eltwise
operation: Convert`. Otherwise `as.double(character_vec)` silently
yields NA.

**A10** Group-by NaN
(`R/query_eval.R:.reject_nan_group_labels` + three call sites):
detect NaN in the grouping vector and raise Julia's
`no IfMissing value specified for the unused entry: NaN`.

**A11** CountBy after mask (`R/query_eval.R:.apply_countby`):
subset `b_per_cell` with `state$indices` so the masked a-side and
the now-masked b-side have matching lengths.

**B5** NaN/Inf comparison RHS
(`R/query_eval.R:.validate_comparator` + `.nan_aware_eq` /
`.nan_aware_neq`): accept the literal tokens NaN/Inf/-Inf at the
validator, and use IEEE-754-faithful equality so `with_nan != NaN`
returns true for every element (NaN != anything is true). The
`!= NaN` fix had to override R's default NA-collapse-to-FALSE
filtering on masks.

**B6 / B7** NaN in eltwise params, Significant on NaN
(`R/operations.R:.require_numeric_param`, `.op_log`,
`.op_significant`, `.significant_vec` + `R/query_eval.R:.coerce_params`):
recognise NaN/Inf as numeric in the param coercer (`as.numeric("NaN")`
yields NaN but `is.na(NaN)` is true; distinguish via `is.nan`),
wrap `if (base <= 0)` / `if (high <= 0)` with `isTRUE()` so NaN
doesn't trip the "missing value where TRUE/FALSE needed" branch
error, and rewrite `.significant_vec` to skip the all-zero fast
path when any input is NaN.

**B8** Float32 sum → Int32 InexactError
(`R/operations.R:.check_inexact_int`): if the strict integer
check fails, round-trip through Float32 (writeBin/readBin
`size = 4`) and re-check. `frac >> Sum type Int32` whose Float64
accumulator drifts to `6.0000000447` now lands cleanly on Int32(6)
- Julia accumulates in Float32 throughout and gets the same result.

**B9** Chained `=@` (`R/query_eval.R:.apply_as_axis` +
the terminal-kind check in `.eval_query`):
`vector_axis` is now a terminal kind. For the validation logic, a
trailing `=@` after a chained lookup (signalled by
`state$pending_final_mask`) is accepted as an annotation; a
top-level `: prop =@` (no prior chain) still requires `prop` to
name an axis. The `<axis>.<suffix>` -> `<axis>` fallback also
applies so `: type.manual =@` works the same way Julia's
`ensure_vector_is_axis` does.

**C5** Min/Max/Mode reject `type`
(`R/operations.R:.op_min` / `.op_max` / `.op_mode` +
`.reject_unknown_param`): give these ops an explicit `type = NULL`
formal so `...` no longer swallows it, then error
`the parameter: type does not exist for the operation: <op>`.

**C6** Matrix-reduction `type` validation
(`R/query_eval.R:.validate_matrix_reduction_type` + 3 dispatchers):
the matrix-reduction fast paths (rowSums / kernel_var_csc / ...)
bypass the per-op .reject_non_float_type guards. Call the
validator at the front door of `.apply_reduction`,
`.apply_reduction_grouped_vector`, and `.apply_reduction_grouped_matrix`
so `>- Mean type Int32` errors instead of silently returning an
integer-rounded vector.

**C7** Significant on Bool input
(`R/operations.R:.op_significant`): when the input is logical,
require `high` / `low` to be exactly 0 or 1 (Julia's
`Bool(0.5)` raises `InexactError`).

**C8** IfMissing range / inexact check
(`R/query_eval.R:.validate_if_missing_default` and
`.coerce_if_missing_default`): range-check Int8 / Int16 / Int32 /
UInt8 / UInt16 / UInt32 literals against `.INT_TYPE_RANGES` at
parse time, regardless of whether the default is consumed. UInt64
rejects negative literals via a string-sign heuristic (Float64
can't represent 2^63 - 1 exactly so bit64-based comparison is
unsafe). Bare integer literals that overflow R's 32-bit `integer`
now promote to `bit64::integer64` so `|| 4294967296` survives.

**D1** Float group-by label format
(`R/query_eval.R:.lc_bool_labels` + new `.julia_float_label`):
whole-numbered Float64 group labels now stringify as `"-1.0"`
matching Julia's `string(-1.0)` rather than dropping the `.0`.
Updated `test-query-count-variants.R`,
`test-query-group-vector-variants.R`,
`test-query-matrix-group-variants.R` whose stale expectations
anchored to R's pre-fix output.

**D3** Hex / binary IfMissing literals
(`R/query_eval.R:.coerce_if_missing_default`):
recognise `0x...` and `0b...` prefixes via `strtoi(..., base = 16/2)`.

**Harness D-class** 0×0 matrix names serialisation
(`dev/adversarial-parity/run_r5.R`): emit an empty list instead
of `NA` when both `nrow == 0` and `ncol == 0`, so the diff matches
Julia's empty-array names output.

**C9** `@ axis [ mask ] : ?` names-on-mask
(`R/queries.R:.validate_query_ast`): a `Names` op (`?`) following a
`BeginMask` / `BeginNegatedMask` is rejected with the same
`invalid query: ...` error Julia produces.

### Still divergent after the implementation pass

These six remain; each is intentional / known.

- **D2** `>- Mean % Log base 2 eps 1` 1-ulp drift
  (`[2.584962]` vs `[2.584963]`). Same Float32/Float64 family as
  A6 in queries.txt - rowMeans on a Float32 matrix accumulates in
  Float64; Julia stays in Float32. Not worth chasing for one ulp.
- **D4** `|| -1e309` overflow: Julia keeps the token as String, R
  parses to -Inf via Float64 overflow. Mirror of the queries4
  finding for `+1e309`. Documented; not worth chasing.
- **C10** `[ label ~ . ]` / `[ ~ .* ]`: Julia's tokeniser rejects
  bare `.` at value position; R passes it through to `grepl`.
  Strict-parser gap; leaving as documented divergence (a user
  writing `~ .` expecting "match anything" would lose useful
  functionality if R also rejected).
- **E5** `@ cell @ cell :: distance @- A` / `@| C`: R extends to
  square-matrix top-level entry-pick; Julia rejects. Pre-existing
  known divergence, documented in FINDINGS.md § E.

### Bug-fix order suggestion (Round 5)

By impact × ease:

1. **A8 (`bool = 0/1` empty)** - 2-line fix in `.coerce_cmp`. Silent
   wrong answer on common Bool masks.
2. **A11 (CountBy after mask)** - subset `b_per_cell` with mask
   indices. Single-spot fix in `.apply_countby`.
3. **A7 (int64 vs fractional literal mask)** - validator-side guard or
   double-cast before bit64 comparison.
4. **A10 (group-by NaN)** - one-line NaN detection in the grouping
   path; raise the same error Julia produces.
5. **A9 (`% Convert type Float` on String)** - input-type guard in
   `.op_convert` (a `.reject_non_numeric_input` helper).
6. **C5 (Min/Max/Mode reject `type`)** - tighten three op signatures.
7. **C6 (matrix `>-` Mean type validation)** - reuse the vector path's
   guard at the matrix-reduction dispatch.
8. **B8 (Float32 → Int32 InexactError)** - widen to Float64 before
   sum-then-cast.
9. **C8 (IfMissing range check skipped)** - validate at parse time, not
   at consume time.
10. **B5 (NaN as mask comparison RHS)** - accept NaN/Inf in
    `.validate_comparator`.
11. **B6/B7 (NaN in eltwise params, NaN-input Significant)** - same
    fix pattern as B5 / Round 4 `% Log` work.
12. **B9 (chained `=@`)** - parser extension.
13. **C7, C9, C10, D1, D2, D3** - small individual fixes, low impact.
14. **D4, B10** - intentional or known; skip.

### Harness for Round 5

```
dev/adversarial-parity/queries5.txt    # 200+ adversarial probes
dev/adversarial-parity/run_julia5.jl   # produces julia_out5.jsonl
dev/adversarial-parity/run_r5.R        # produces r_out5.jsonl
dev/adversarial-parity/diff5.py        # diffs *_out5.jsonl files
```

NB: `julia --project=$HOME/src/DataAxesFormats.jl` no longer works with
the system default Julia 1.11.8 — the DataAxesFormats manifest was
resolved with 1.12.5. Use
`/home/aviezerl/tools/.julia/juliaup/julia-1.12.5+0.x64.linux.gnu/bin/julia`
explicitly until `juliaup` is reconfigured.

---

## Round 6 - centralized validator + grammar-driven fuzzer (2026-05-15)

Rounds 1-5 were hand-crafted adversarial queries. The bug density per
*previously-untested* region kept hitting ~25% (Round 5 found 48 new
divergences in 200 probes), suggesting the DSL surface area is too
large for hand-rolled queries to sweep. Round 6 swaps methodology:

1. **Grammar-driven fuzzer** (`dev/adversarial-parity/fuzz/`):
   `gen_queries.py` emits 500-1000 random valid queries per seed,
   biased toward edge-case patterns (NaN inputs, type tags, masks,
   group-by NaN/float, IfMissing typed defaults, lowercase aliases,
   hex/binary literals). The grammar mirrors the actual parser.
2. **Triage** (`fuzz/triage.py`) buckets divergences by error-keyword
   class so 200 instances of "the same bug" surface as one row.
3. **Centralized op validator** (`R/op_dispatch.R`): one per-op
   metadata table (`.OP_META`) + one `.validate_op_invocation()`
   called from every dispatcher (eltwise, reduction-to-scalar,
   matrix-reduction, grouped-vector, grouped-matrix) BEFORE
   fast/slow path selection. Replaces scattered `.reject_non_float_type`
   / `.reject_non_number_type` / `.reject_unknown_param` calls
   formerly duplicated across 4-5 dispatchers.

### Methodology

```
dev/adversarial-parity/fuzz/gen_queries.py   # grammar-driven generator
dev/adversarial-parity/fuzz/triage.py        # bucket-by-error-class

# Pipeline:
python3 fuzz/gen_queries.py --n 1000 --seed 42 > queries6.txt
julia run_julia6.jl       # → julia_out6.jsonl
Rscript run_r6.R          # → r_out6.jsonl
python3 fuzz/triage.py    # bucket + dedupe + report
```

Three seed runs (500 / 500 / 1000) covered different surface area;
each surfaced 5-12 real divergences (jl_err_r_ok / jl_ok_r_err /
value diffs) plus ~270 cosmetic message-format diffs that, with
alignment fixes, collapsed to one shared error class per language.

### Centralized validator architecture

Before Round 6: each dispatcher had its own param validation. For
example, the matrix-reduction fast path (`>- Mean type Int32`)
bypassed the vector `>>` path's `.reject_non_float_type` guard that
Round-5 added; the grouped-vector path bypassed both; the dry-call
hack in `.apply_eltwise` only caught value-range checks, not the
input-type guard. Each new op required wiring through 4-5
dispatchers, and the fuzzer kept finding "this dispatcher missed
the guard the other one has."

`R/op_dispatch.R` introduces:

- `.OP_META` - per-op declaration of: input-type rejection rule
  (`accepts_string_input`), type-param policy (`none` / `float` /
  `number` / `required:number`), and declared named params
  (everything else triggers `the parameter: <X> does not exist`).
- `.validate_op_invocation(op, kind, input, params)` - runs the
  three checks (type-param, input-type, declared-name) in Julia's
  validation order (parse_query validates the type tag BEFORE
  property lookup, so the type-tag error surfaces first). Called
  at the front door of every dispatcher.
- `.validate_op_invocation_at_parse(op, kind, params)` - parse-time
  slice that runs only checks not needing the input value, called
  from `.parse_eltwise` / `.parse_reduction`. Surfaces type-tag
  rejection during parse, before any axis/property lookup at eval.

After centralization:
- Wiring a new op needs one entry in `.OP_META`, not 4-5 dispatcher
  edits.
- Removing a `.reject_unknown_param` from an op fn (e.g. removing
  `low`/`high` synonyms for Clamp to match Julia) is a one-line
  metadata change.
- The dry-call hack is retained for value-range checks the op fn
  owns (e.g. `% Log base 0`); but type/input checks no longer
  depend on it.

### Bugs surfaced by the fuzzer (and fixed)

Each is a real R bug the central validator + grammar fuzzer caught
that earlier hand-rolled rounds missed.

- **`% Convert type float64` on `% Fraction`** - `.op_fraction`'s
  hard-coded float-type list was case-sensitive. Replaced with the
  central `.reject_non_float_type` (which canonicalises lowercase).
- **`% Significant high Infinity` on Int matrix** - integer-input
  Significant must reject non-finite or out-of-range high/low with
  `InexactError: Int32(Inf)`. Added bounds + NaN/Inf detection in
  `.op_significant`.
- **`% Abs type bool` on `infs`** - `.check_inexact_bool` only
  rejected fractional finite values, not Inf/NaN. Extended.
- **`|| e UInt16`** / **`|| -Inf Int64`** - `.validate_if_missing_default`
  short-circuited on named constants without type-checking them
  against the declared int range. Now: named-constant + int target
  rejects non-integer-valued constants (e, pi, NaN, Inf).
- **`|| 0b10 Int8`** / **`|| 0x10 UInt32`** - typed integer
  IfMissing path's regex only matched decimal digits. Added
  hex/binary handling in `.validate_if_missing_default` and
  `.strict_int_coerce`.
- **`% Significant high e`** - the op had its own `is.numeric(high)`
  check that rejected named constants. Routed through
  `.require_numeric_param` + `.resolve_named_numeric` for parity
  with `% Log`.
- **`>- StdN eps e`** - same root cause on the matrix-reduction
  fast path's `.param_eps`. Fixed.
- **`% Quantile p e`** - same, in `.op_quantile`.
- **Group-by NaN with IfMissing** - my Round-5 A10 fix rejected NaN
  groups unconditionally. Julia accepts NaN as a group label when
  IfMissing is supplied, filling the NaN bucket with the default.
  Moved the rejection from group-by time to reduction time so
  IfMissing can be observed; added a finalize-step substitution that
  replaces NaN-bucket reduction values with the IfMissing default.
- **`@ cell [ missing_prop ~ pat ]` "comparator outside of mask"** -
  R deferred BeginMask to the matrix-mask path when the property
  wasn't a vector, but the next AST node was a comparator (not
  `@ axis = entry`), surfacing as the wrong error. Added eager
  rejection: if the property isn't a vector and no matrix has the
  same name on any axis, error `missing vector: <name>` like Julia.
- **`% Clamp low/high` synonyms** - dafr historically accepted
  `low`/`high` as Clamp param synonyms; Julia accepts only
  `min`/`max`. Removed from the query-DSL accepted-name list (via
  `.OP_META`); the R-API direct function call still works.

### Error-message format alignment

The bulk of "both_error_mismatch" buckets were the same root error
in both languages with different wording. Aligning the wording made
hundreds of buckets collapse to `both_error_aligned`:

- `.reject_unknown_param` - emit `the parameter: <X>\ndoes not
  exist for the operation: <Y>` (matches Julia's `\n` exactly).
- `.parse_eltwise` / `.parse_reduction` - same `\n` format for the
  parse-time unknown-param error.
- `.op_significant` "high required" - emit `missing required
  parameter: high` (matches Julia's format for required params).
- `.op_convert` missing-type - emit `missing required parameter:
  type` (was `Convert: 'type' parameter is required (one of ...)`).
- `.op_quantile` missing-p - emit `missing required parameter: p`.
- `.parse_cmp` missing-value - emit `expected: value` (was
  `expected value after comparator at position N in query ...`).
- `.validate_comparator` IsMatch on non-string - drop the
  `for the comparison operation: <op>` suffix to match Julia's
  shorter wording.

### Round 6 final state (1000-query fuzzer run, seed 31337)

```
both_error_aligned    503    # same root error, same wording
both_error_mismatch   271    # same root error, residual wording diffs
match                 204
kind_mismatch          11    # set_or_strvec vs vector (D-class, known)
jl_err_r_ok             6    # see below
vector_val_diff         3    # Mode tie-break (known) + 1 edge case
vector_name_diff        1    # Float32 sub-axis name formatting
matrix_diff             1    # empty-mask matrix shape
                     ----
                     1000
```

Real divergence rate: ~1.1% (11 of 1000). Down from ~24% in Round 5
hand-rolled probes. The remaining `jl_err_r_ok` cases are mostly:
- Julia rejecting unusual operator combinations with `invalid
  operation(s)` (e.g. mask with 3-level XOR, IfMissing on `% Abs`
  when output type doesn't fit the default)
- Julia internal `MethodError` on `Quantile` matrix paths (Julia
  side bug, R produces the right answer)
- Mode tie-breaking on integer matrices (already documented)

These are edge cases. The systematic gaps (type validation, NaN
semantics, IfMissing range checks, error format alignment) are
closed.

### Rounds 1-5 regression status

After all Round-6 changes, rounds 1-5 harness output unchanged from
before Round 6: same match / both_error counts, same documented
remaining divergences. The central validator refactor is a pure
re-organisation; it removes redundancy without changing behaviour
on the existing test suite (1 pre-existing fail in
`test-operations-registry.R:117`, no new regressions).

### Harness for Round 6

```
dev/adversarial-parity/fuzz/gen_queries.py     # grammar fuzzer
dev/adversarial-parity/fuzz/triage.py          # bucket-by-class diff
dev/adversarial-parity/queries6.txt            # latest seed (regenerable)
dev/adversarial-parity/run_julia6.jl
dev/adversarial-parity/run_r6.R
```

To re-run with a new seed:
```
python3 fuzz/gen_queries.py --n 1000 --seed 12345 > queries6.txt
julia run_julia6.jl
Rscript run_r6.R
python3 fuzz/triage.py
```

---

## Harness reuse

```
dev/adversarial-parity/
├── build_fixture.jl   # one-time Julia run to (re)create the FilesDaf
├── queries_all.txt    # 320 queries, # comments + blank lines allowed
├── run_julia.jl       # parse_query → get_query → julia_out.jsonl
├── run_r.R            # devtools::load_all → get_query → r_out.jsonl
├── diff.py            # diffs the two JSONL files, prints summary + diffs
└── fixture.daf/       # the shared on-disk DAF
```

To re-run after fixes:

```
julia --project=$HOME/src/DataAxesFormats.jl run_julia.jl
Rscript run_r.R
python3 diff.py
```

To add a query: append a line to `queries_all.txt` and re-run.

## E. 2026-06-10 re-sweep (post 0.3.0 packed/1.1 work)

Re-ran adversarial R-vs-Julia probes against current dafr. Confirmed FIXED (no
regression): A2 (`% Round type Int8` on integer64 now returns correct values),
A5 (`>> Median` with NaN returns NaN, not NA). Validated the NEW packed reader
across every dtype (Int8..UInt64, Float32/64), all-true Bool sparse, NaN/Inf/
-Inf, and vlen-utf8 unicode/control-char strings - all correct.

### E1. `>> Median` of a vector with BOTH NaN and +-Inf  (NEW, narrow)
`Median([1, NaN, 3, Inf, -Inf])`: dafr = **NaN**, DAF.jl = **-Inf**. Every other
reduction (Mean/Sum/Max/Min/Var/Std) on this input is NaN in BOTH; all-Inf,
NaN-only, and ordinary inputs match exactly. R's NaN-propagating median vs
Julia's sort-based median kernel diverge only on the NaN+Inf mix. R's NaN is
arguably the more correct answer; matching -Inf needs reimplementing Julia's
exact median ordering in the C kernel - deferred (same class as prior median/
quantile kernel divergences). Pinned by tests/testthat/test-reductions-special-values.R.
