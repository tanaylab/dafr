# Audit: views.jl literal-parity divergences

Date: 2026-05-07
Driver: literal port of `~/src/DataAxesFormats.jl/test/views.jl` (654
lines, ~50 nested_test leaves) into
`tests/testthat/test-views-jl-parity.R`.

The port surfaced no inline-fixable behavior bugs, but did surface
several semantic divergences in dafr's view layer that warrant
follow-up. View semantics is a heavily Julia-flavored area
(`__axis__` placeholder, tensors, strict-include-list defaults), and
several tests required Julia features dafr doesn't have.

## Status

- **Fixed inline:** none.
- **Open / skipped:** V1 (no tensor support in viewer), V2 (permissive
  wildcard validation), V3 (data items add to default-all-visibility,
  not strict-include-list), V4 (query `:: UMIs % Op` rejected by
  evaluator), V5 (no `__axis__` placeholder), V6 (`:: UMIs` query with
  no leading axes rejected by parser), V7 (no scalar-shape validation
  on view-scalar resolution). Plus C2 (description deep parameter,
  same as chains slice).

Skip count in `test-views-jl-parity.R`: 17 across 8 unique IDs (V1-V7
+ C2). Result: `FAIL 0 | SKIP 17 | PASS 33`. Higher skip ratio than
prior parity slices because views.jl is a heavy view-construction-
error file plus has Julia-specific features.

---

## Open divergences

### V1. dafr's view layer has no tensor support

- **Symptom.** Julia treats matrices named `<entry>_<suffix>` (where
  `<entry>` is from a "main" axis) as a virtual tensor — `description`
  emits a `tensors:` block, viewer accepts a 4-tuple key `("batch",
  "cell", "gene", "is_high")` to pin a tensor pattern, and
  `matrices_set(d, ra, ca; tensors = false)` toggles the rollup.
  dafr has tensor concept in contracts (`R/contracts.R`) but the view
  layer has no tensor handling — viewer rejects 4-tuple keys, and
  `description()` doesn't roll matrices up into tensors.
- **Tests guarded.** Whole `views / tensor / *` group (1 stub skip
  covering 8 leaves).
- **Fix sketch.** Substantial. Would need (a) a tensor-detection pass
  in `viewer()` that recognizes the `<axis>_<entry>` naming
  convention, (b) a 4-tuple-key parser in `.parse_view_item`, (c)
  `description()` rolldown logic plus `tensors = false` parameter.
  ~150-200 lines. Not blocking for any current dafr use case.

### V2. Viewer is permissive on wildcard query positions

- **Symptom.** Julia rejects `data = ["*" => "version"]` (a wildcard
  scalar key with non-`=`, non-`nothing` query) at viewer construction
  with "invalid wildcard scalar query: version / query for wildcard
  must be one of: '=', nothing". dafr accepts and stores the query
  silently, only failing later (or not at all). Same for vector
  `("cell", "*") => "age"` and matrix `("*", "gene", "UMIs") =>
  "UMIs"` — Julia rejects, dafr accepts.
- **Tests guarded.** `views / scalar / !*`, `views / vector / !* /
  {axis, property}`, `views / matrix / !* / {rows_axis, columns_axis,
  property}` — 6 skips.
- **Fix sketch.** In `.resolve_view_scalars` / `.resolve_view_vectors`
  / `.resolve_view_matrices`, when an item's key contains `*` in the
  wildcard position(s), validate that the value is `"="` or `NULL`
  and stop with a Julia-shaped error otherwise. ~30 lines.

### V3. Data items add to default-all-visibility; Julia uses strict include list

- **Symptom.** dafr's view seeds `view_vectors` with EVERY base
  vector from every renamed axis (`R/view_daf.R::.resolve_view_vectors`
  lines 292-302), then layers data items on top. So `data =
  [(cell, age) => "="]` exposes ALL vectors plus an explicit override
  for cell|age. Julia's semantics: data items are a strict include
  list — `data = [(cell, age) => "="]` exposes ONLY cell|age (nothing
  else). Same for matrices.
- **Tests guarded.** `views / vector / hidden / explicit-only`,
  `views / vector / renamed`, `views / matrix / hidden /
  explicit-only` — 3 skips. The masked-axis-with-explicit-vector test
  was relaxed to assert the substantive result instead of the
  vectors_set strict-include question.
- **Fix sketch.** Behavioral change with breakage potential —
  existing R-side users relying on dafr's seed-all default would see
  views shrink. Either add a flag (`include_all = TRUE` legacy
  default, FALSE for Julia parity) or version-bump and make Julia
  parity the new default. Worth a user discussion before changing.

### V4. Query `:: <matrix> % <op>` rejected by evaluator

- **Symptom.** Julia accepts `:: UMIs % Abs` (matrix lookup followed
  by element-wise op) as a valid query, returning the abs-applied
  matrix. dafr's evaluator errors at the `% Abs` step with
  `'%' eltwise requires scalar, vector, or matrix in scope`. The
  matrix-from-`::` lookup may not put the matrix in the right "scope"
  state for the eltwise dispatch.
- **Tests guarded.** `views / matrix / query` — 1 skip.
- **Fix sketch.** `R/query_eval.R::.apply_lookup_matrix` should
  transition state to a kind that the eltwise op accepts. Targeted
  evaluator fix, ~10 lines once the cause is pinpointed.

### V5. Query DSL has no `__axis__` placeholder

- **Symptom.** Julia's view-data queries can use `__axis__` as a
  literal axis-self-reference: `data = [("cell", "total_UMIs") =>
  "@ gene @ __axis__ :: UMIs >- Sum"]` resolves `__axis__` to `cell`
  at query eval. dafr's parser sees `__axis__` as an unknown axis
  name. Used by `views/vector/masked/query` (substantive case) and
  `views/vector/matrix` (error-flow case).
- **Tests guarded.** `views / vector / masked / query`, `views /
  vector / matrix` — 2 skips.
- **Fix sketch.** Add a query-source pre-substitution pass in the
  view's vector / matrix lookup methods that replaces `__axis__` (or
  `__rows_axis__` / `__columns_axis__`) with the relevant axis name
  before passing to `get_query`. ~20 lines per call site.

### V6. Query `:: UMIs` (no leading axes) rejected

- **Symptom.** Julia accepts a matrix-lookup query with NO leading
  `@ rows @ cols` when the query is used as a view's matrix-slot
  resolver (the slot's rows / cols axes provide the context). dafr's
  parser rejects: "invalid query: :: UMIs" — it requires the leading
  axes to be in scope.
- **Test guarded.** `views / requires_relayout / realized` — 1 skip
  (formerly a substantive test that would have exercised the
  renamed-axis matrix lookup).
- **Fix sketch.** Pairs with V5: the view-matrix dispatch could
  pre-prepend `@ <view_rows_base> @ <view_cols_base>` to the user
  query before parsing, so the user can write `:: UMIs` and the
  view layer fills in the axes.

### V7. View-scalar layer doesn't validate scalar-shape result

- **Symptom.** `data = [("sum_ages", "@ cell : age")]` — a query that
  returns a vector. Julia errors: "vector query: @ cell : age / for
  the scalar: sum_ages / ...". dafr silently returns the vector
  through `get_scalar`, breaking the scalar contract.
- **Test guarded.** `views / scalar / vector` — 1 skip.
- **Fix sketch.** `R/view_daf.R::format_get_scalar` for `ViewDaf`
  should call `length(result) == 1L` post-eval and stop with a
  parity-shaped error if not. ~5 lines.

### C2 (carry-over from chains slice). `description(...; deep)` not supported

- Same as the chains-slice C2. `views/requires_relayout/{(),deep}`
  skipped as 2 stubs.

---

## R-fundamental / non-portable

### T1. Construction-time-vs-access-time error timing

Several tests assert `expect_error(axis_vector(view, ...))` after
constructing a view from an invalid axis-query. dafr errors at
construction (during axis-index resolution), Julia errors at access.
The error is the same logical error; the timing differs. Tests
relaxed to `expect_error(viewer(...))` where dafr surfaces it earlier.
Affected: `views/axis/scalar`, `views/axis/vector`.

### T2. Error wording

Same shape as concat / reorder / chains slices. dafr's error wording
is single-line, Julia's is chomp-formatted multi-line. Regex looks
for tokens.

---

## Test catalog

`tests/testthat/test-views-jl-parity.R` — 50 `test_that` blocks,
mirroring views.jl's nested_test tree. The tensor group is one
collapsed skip covering 8 leaves; description-deep tests are 2 skips.

Counts:
- Behavior bugs fixed inline: 0
- Open divergences guarded by skip: 8 unique IDs across 17 skips (V1
  ×1 stub-for-8-leaves, V2 ×6, V3 ×3, V4 ×1, V5 ×2, V6 ×1, V7 ×1, C2
  ×2)
- T1 / T2: relaxations rather than skips
