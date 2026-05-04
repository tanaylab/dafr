# Slice 1b — Julia parity quick wins (E4 / E5 / E10 / B7 / B9 / API1) — Exit note

**Date:** 2026-05-04
**Branch:** `slice-julia-parity-1b` off `dev` post-Slice-1a (`4102d80`).
**Predecessor:** `slice-julia-parity-N1-exit.md`.
**Successor:** Slice 2 — E3 / E7 / E8 / E11 (medium evaluator).

## Scope delivered

Six divergence IDs closed; one bundled slice because each fix is small and
the tests share the `test-queries-jl-parity.R` skip-marker pattern.

- **E4** — top-level comparator after `:` / `::`. Already worked in the
  evaluator; un-skipped 7 tests. No code change.
- **E5** — `:` / `::` standalone (top-level entry-pick) with IfMissing
  fallback. **Bug fix.** `.eval_query` resets `state$if_missing` on the
  new state after every lookup dispatch, which clobbered the IfMissing
  default that pending entry-pick states (`vector_entry_pending_axis`,
  `matrix_entry_pending_first_axis`) had captured for later use. Renamed
  the captured field to `pending_if_missing` / `pending_if_missing_type`
  in those pending states, and updated `.apply_top_level_vector_entry`
  and `.apply_top_level_matrix_entry` to read those keys. `: age || 1 @
  cell = X` and `:: UMIs || 0 @ cell = Y @ gene = B` now return their
  defaults when the property is missing.
- **E10** — regex escape sequences in masks. Already worked
  (cumulative parser-strictness fixes). Un-skipped.
- **B7** — `Sum()` / `Mean()` / `Max()` / `Min()` / `Mode()` /
  `Median()` / `Count()` / `GeoMean()` / `Quantile()` / `Std()` /
  `StdN()` / `Var()` / `VarN()` builders. **Bug fix.** All thirteen now
  emit `ReduceToScalar` AST nodes (`>> Sum` canonical) instead of
  `Eltwise` nodes (`% Sum`, which the runtime then refused with
  `unknown eltwise operation`). New `.qop_reduction_typed` helper
  parallels the existing `.qop_eltwise_typed`. `.make_reduce_to`
  accepts both `ReduceToScalar` and (legacy) `Eltwise` trailing nodes
  when rewrapping. `test-builders-reductions.R`'s 37 canonical-string
  assertions flipped from `% X` to `>> X`.
- **B9** — `query_axis_name` introspection strictness. **Bug fix.**
  `R/queries.R::query_axis_name` walked every `Axis` node in the AST,
  including those inside `[ ... ]` mask sub-queries — so compound-mask
  queries like `@ cell [ is_low & UMIs @ gene = B ]` returned `NA`
  (two axes seen). New version maintains a depth counter and only
  counts outer-scope axes. The `get_result` test helper in
  `test-queries-jl-parity.R` no longer needs its `tryCatch` workaround.
- **API1** — `get_dataframe` named-list column-spec shorthand.
  **Small fix.** Named queries (`list(age = ":age")`) and complex
  axis-traversal forms already worked. The bare-name shorthand
  (`list("age", doublet = "is_doublet")`) didn't auto-prefix and ran
  the literal "age" through `get_query`, which errors. Extended
  `R/dataframes.R::.apply_dataframe_columns` to detect a bare property
  name (matches `^[[:alnum:]_.]+$`) and rewrite it to `@ axis : <name>`.

## Cumulative effect

Crucially, un-skipping the lump-skipped 48 `E5-E11` tests revealed that
**39 of them already pass** off of the cumulative B1-B6 / P1-P5 / E1-E2
/ B7 / E5 / N1 fixes. They were skipped behind a stale lump-skip
message. The 9 that genuinely fail map to `E3` / `E6` / `E8` / `E11`
(out-of-scope here) plus 3 T-class error-text divergences. Each was
re-skipped with a sharper ID so future slices can find them by ID.

## Numbers

- **Pre Slice 1b:** `FAIL 0 | WARN 1 | SKIP 72 | PASS 4495` (post-1a).
- **Post Slice 1b:** `FAIL 0 | WARN 1 | SKIP 14 | PASS 4603`.
- Net: -58 skips, +108 passes.

## Files touched

- `R/query_eval.R` — `pending_if_missing` rename in `.apply_lookup_vector`
  and `.apply_lookup_matrix` init branches; matched read in
  `.apply_top_level_vector_entry` and `.apply_top_level_matrix_entry`.
- `R/query_ast.R` — `.qop_reduction_typed` helper; thirteen reduction
  builders (`.qop_sum`, `.qop_mean`, `.qop_max`, ..., `.qop_var_n`)
  switched from `.qop_eltwise[_typed]` to `.qop_reduction_typed`.
- `R/query_builders.R` — `.make_reduce_to` accepts both
  `ReduceToScalar` and `Eltwise` trailing nodes.
- `R/queries.R` — `query_axis_name` skips mask-internal `Axis` nodes.
- `R/dataframes.R` — bare-name auto-prefix in
  `.apply_dataframe_columns`.
- `tests/testthat/test-builders-reductions.R` — 37 canonical-string
  assertions flipped from `% X` to `>> X`.
- `tests/testthat/test-queries-jl-parity.R` — un-skipped E4 / E5 / E10
  / B7 / API1 tests; tightened `get_result` helper (B9); re-skipped 9
  remaining failures with sharp E-IDs.

## Follow-ups handed forward

- **Slice 2** (E3, E7, E8, E11): matrix-slice-as-mask, group/count-by
  matrix slices, cross-tabulate, as_axis with `=@`.
- **Slice 3** (E6, E9): matrix-then-vector lookup chains, auto-relayout.
- T-class error-text divergences may be reconcilable with custom error
  wrappers around the offending operations — recorded but not on any
  slice's docket.
