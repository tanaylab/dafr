# Audit: R-only quirks not surfaced by ported Julia tests

Date: 2026-05-03
Driver: ~/temp/AUDIT-r-quirks-vs-julia-tests.md
Branch: dev (worktree `.worktrees/parser-slice`)

The audit was prompted by `ba9baa7 / fff46f2` (`.as_booleans`): a
class-of-bug where R coerces character vectors silently when compared
with numeric `0`, hiding empty-string sentinels that Julia handles
correctly. Hypothesis: there are sibling cases — places where a
ported Julia test would not exercise the type combination R can
produce but Julia cannot.

## Findings (genuine parity gaps)

### F1. `[ prop < value ]` mask returns NA on factor properties — FIXED

- **Quirk.** `vec < "x"` on an unordered factor returns NA with a
  warning; on an ordered factor it compares level codes (not labels).
  Julia's `compute_comparison(IsLess, ...)` uses `<` on
  `StorageScalar`, which is lexicographic for strings.
- **Sites.**
  - `R/query_eval.R:.apply_comparator` — uses `state$pending_vec`
    directly in `vec < x` etc.
  - `state$pending_vec` is set by `.apply_begin_mask` and
    `.apply_logical_mask` from `format_get_vector(...)`, which can
    return a factor (e.g. h5ad categoricals via
    `R/anndata_format.R:.read_h5ad_categorical`).
- **Julia reference.** `~/src/DataAxesFormats.jl/src/queries.jl:2261-2400`
  (`compute_comparison(::IsLess, …)` and friends).
- **Test.** `tests/testthat/test-query-mask-factor.R` — covers `<`,
  `>`, `<=` on unordered factor; `<` on an ordered factor whose level
  codes invert the lexical order; sanity tests for `=` and the truthy
  mask.
- **Fix.** `b866941` — added `.normalize_pending_vec` next to
  `.as_booleans` and applied it to `state$pending_vec` in both mask
  helpers.

### F2. `>> Mode` on character / factor errors out — FIXED

- **Quirk.** `.op_mode` rejected non-numeric/non-logical input.
  Julia's Mode is documented as supporting strings (and CategoricalVector
  is normalized to `Vector{String}` at the storage boundary). The
  grouped path `.apply_reduction_grouped_vector` only routed to the
  dedicated `.grouped_mode_character` helper for `is.character(x)`,
  so factor properties fell through to a slow fallback that re-called
  `.op_mode` and errored.
- **Sites.**
  - `R/operations.R:378` — `.op_mode` rejected character.
  - `R/query_eval.R:1195` — grouped Mode early-route excluded factor.
- **Julia reference.** `~/src/DataAxesFormats.jl/src/operations.jl:1058-1115`
  (`Mode <: ReductionOperation`, `supports_strings(::Mode) = true`)
  and `anndata_format.jl:403` for the categorical → Vector{String}
  normalization.
- **Test.**
  - `tests/testthat/test-operations-reductions.R` — replaced the
    obsolete "Mode raises on character" guard with positive cases
    for character + factor.
  - `tests/testthat/test-operations-query.R` — added `>> Mode` on a
    character property and on a factor property (reversed levels).
  - `tests/testthat/test-query-grouped.R` — added grouped factor case.
- **Fix.** `364061f` — `.op_mode` accepts character; coerces factor
  to character at entry. Grouped early-route extended to factor. Old
  error message replaced with a more accurate one.

## Non-issues (checked, no fix needed)

Documented so the next quarter's audit doesn't re-walk these paths.

### N1. `vec != 0` / `vec == 0` outside `.as_booleans`

Five sites total. All gated by an `eltype != "String"` check or a
sparse-x slot (numeric by construction):

- `R/query_eval.R:485` — inside `.as_booleans`; factor / character
  branches handled.
- `R/files_io.R:311` — `.should_sparsify_numeric`; only invoked
  when the dispatcher has already chosen the numeric branch.
- `R/files_daf_write.R:232` — same dispatch site as above.
- `R/operations.R:215` — operates on `x@x` of a `dgCMatrix`, which
  is numeric.

### N2. `match()` without `nomatch =`

18 call sites. Each is either:

- post-checked with `is.na(idx)` and a domain-specific error
  (`.apply_chained_lookup_vector`, `.apply_pick_entry`, square
  slice) — or
- guarded by an upstream relation invariant
  (`copies.R:.copy_vector` only matches when
  `relation == "destination_is_subset"`; `dataframes.R` only
  matches the result of an upstream `format_axis_array` subset).

`R/contracts.R:1056` is the one site with an explicit `is.na(j)`
fallthrough branch.

### N3. `1:n` indexing

None present. `seq_len(n)` / `seq_along(x)` already used everywhere
in `R/`.

### N4. `as.numeric()` on factor inputs

39 sites; all operate on:

- numeric / `Matrix` / sparse `dgCMatrix` slots (matrix slicing,
  rowsum, fast-path log) — or
- parameter strings parsed from the query DSL
  (`.coerce_cmp`, `params$eps`, `Quantile p`).

No factor flows in.

### N5. `is.character() & !nzchar()` empty-string detection in chained lookup

`R/query_eval.R:426-427`. For factor pivot values the
`is.character()` branch is FALSE so the explicit empty-string check
doesn't fire. **However:** axes are forbidden to contain `""` —
asserted in `R/memory_daf.R:151`, `R/files_daf_write.R:51`,
`R/files_daf_read.R:64`. So `match("", target_entries)` always
returns `NA`, and the `is.na(indices)` branch on the line above
catches every empty case regardless of whether the pivot was
character or factor. The `is.character() & !nzchar()` clause is
defensive belt-and-suspenders only.

### N6. `sum(x)` in `.op_fraction` (no `na.rm`)

`R/operations.R:253` — `total <- sum(x)`. With `NA` in the input
this produces `total = NA`, and `total == 0` evaluates to `NA`,
which makes `if (total == 0)` error with "missing value where
TRUE/FALSE needed". Julia's `compute_eltwise(::Fraction, ::StorageVector)`
also `sum(input)` without skipping NaN, but Julia data has no
NA semantics — NaN propagates through. Not a parity gap; both
languages "fail" symmetrically on missing input. DAF convention is
`""` / `0` for missing, so this code path doesn't see NA in
practice.

### N7. Integer overflow in reductions

`R/operations.R:.op_sum` — `sum(integer)` overflows to `NA_integer_`
with a runtime warning. The DSL exposes `>> Sum type Int64` /
`type Float64` for explicit promotion (forwarded into `.op_sum`'s
`type` parameter), and the warning is loud. Not a silent bug.

### N8. h5ad write / read symmetry

`R/anndata_format.R:.read_h5ad_categorical` returns a factor;
`.write_h5ad_categorical` writes any factor as a `categorical`
group. Symmetric. Julia DAF explicitly normalizes
`CategoricalVector` → `Vector{String}` at the storage boundary
(`anndata_format.jl:403`); dafr keeps factors. Both Mode (F2) and
the comparator path (F1) now coerce factor → character at the
consumer, which matches Julia's effective semantics without
forcing the storage boundary to lose `levels`. If a future change
wants tighter parity, the boundary is `R/utils.R:.validate_vector_value`
— that's the single chokepoint to flip.

## Out of scope (not chased this pass)

- Performance auditing (separate pass per `inst/benchmarks/`).
- Cross-version Julia compatibility.
- Sparse-vs-dense densification audits (silent type changes when
  arithmetic between sparse/dense hits a dispatched method).
- UTF-8 / encoding audits — none of dafr's tests run on Windows
  with non-ASCII data, but the platform-specific branches are
  identical across kernels so no concrete bug to chase here.

## Methodology recap

1. Greps from the audit doc, scoped to `R/` and `src/`:
   `vec != 0` family, `as.numeric`, `1:n`, `match\(` w/o nomatch,
   `sum/prod` w/o `na.rm`, `unlist`, `do.call(rbind|cbind, ...)`,
   `is.character\(`.
2. Each hit cross-referenced to the Julia source it ports
   (`~/src/DataAxesFormats.jl/src/`).
3. Genuine gaps converted to a one-finding-one-commit fix with at
   least one targeted testthat test demonstrating the bug.
4. Non-issues recorded above so the next quarter doesn't re-walk.

Branch state at end of pass: `dev` ahead of `private/dev` by 4
commits (`AGENTS.md`, F1, F2, this audit note).
