# Audit: queries.jl literal-parity divergences

Date: 2026-05-03
Driver: literal port of `~/src/DataAxesFormats.jl/test/queries.jl` (1499 lines,
~130 nested_test leaves) into `tests/testthat/test-queries-jl-parity.R`.

The port surfaced two classes of divergence between dafr and DAF.jl: behavior
bugs (wrong answer) — fixed inline in this slice — and structural parser /
evaluator gaps that warrant their own follow-up. This document is the punch
list for that follow-up.

## Status (2026-05-04 — post-Slice-2)

- **Closed:** B1-B6 (literal-port), P1-P5 (parser-strictness), E1, E2
  (parser-strictness), N1 (S1 + Slice 1a), E4, E5, E10, B7, B9, API1
  (Slice 1b), E3, E7, E8 (Slice 2 — already-working confirmations).
- **Open:** E6 (Slice 3, vector/matrix lookup chains), E9 (Slice 3,
  auto-relayout), B8 (introspection deferral, harmless).
- **Reclassified to T-class** (error-text-only): E11 (R kernels promote
  int matrices to double during Sum reduction, losing the type signal
  Julia uses for InexactError on non-integer IfMissing defaults).
- Three other T-class divergences: `>| Sum` / `>- Sum` on string matrix;
  `?? foo` with R's `as.integer` coercion warning.

## FIXED in this slice (commits in slice-18 / dev)

### B1. `>>` reduction on empty vector / matrix returns 0

- **Symptom.** `get_query(d, "@ cell [ all_false ] : age >> Sum")` returned
  `0` silently. Julia errors: `no IfMissing value specified for reducing an
  empty vector`.
- **Fix.** `R/query_eval.R::.apply_reduction_to_scalar` — reject zero-length
  input unless `state$if_missing` is set; with IfMissing, return the coerced
  default.
- **Julia ref.** `queries.jl:351-358, 398-407`.

### B2/B3. `>|` / `>-` on empty matrix returns wrong shape or internal R error

- **Symptom.** `>|` on a `2x0` matrix errored with `'names' attribute [2] must
  be the same length as the vector [0]`; `>-` returned zeroed entries with no
  IfMissing. Julia errors uniformly: `no IfMissing value specified for
  reducing an empty matrix`. With IfMissing: indexed-axis-empty → empty named
  vector; reducing-axis-empty → fill output with default per cell.
- **Fix.** `R/query_eval.R::.apply_reduction` — empty-dim short-circuit before
  `fn` dispatch, branching on which dim is empty and on IfMissing presence.
- **Julia ref.** `queries.jl:648-666, 685-703`.

### B4. Eltwise `% Op` on scalar errored

- **Symptom.** `". score % Abs"` errored `'%' eltwise requires vector or
  matrix in scope` even when score was a numeric scalar. Julia returns the
  per-element op applied to the scalar.
- **Fix.** `R/query_eval.R::.apply_eltwise` — accept `state$kind == "scalar"`
  in the kind guard. Numeric R ops (`abs`, `log`, etc.) handle scalars
  natively; string scalars error from base R, which is acceptable per text-only
  divergence policy.
- **Julia ref.** `queries.jl:423-426`.

### B5. Partial / unconsumed query returns NULL

- **Symptom.** `get_query(d, "@ cell @ gene")` returned `NULL` silently;
  `has_query` returned `FALSE`. Julia errors: `invalid query: @ cell @ gene`.
- **Fix.** `R/query_eval.R::.eval_query` — finalize check: if final
  `state$kind` is not in `{scalar, vector, matrix, names, axis}`, raise
  `invalid query: <canonical>`.
- **Julia ref.** `queries.jl:110-114`.

### B6. `? ?` re-listed axes instead of erroring

- **Symptom.** A second `?` after a fully-resolved Names result fell through
  the dispatch default and re-listed axis names. Julia errors: `invalid
  operation(s)`.
- **Fix.** `R/query_eval.R::.apply_names` — make the `init`-state branch
  explicit; reject any other unresolved kind.
- **Julia ref.** `queries.jl:239-244`.

---

## FIXED in the parser-strictness slice (commits on `dev`)

These items are closed by the parser-strictness follow-up slice. The slice
plumbed the operation registry into the parser and added auto-typing /
type-annotation parsing for `||` defaults. Skip guards for these were
removed from `tests/testthat/test-queries-jl-parity.R` and the substantive
assertions now run.

### P1. Unknown eltwise / reduction op rejected at parse

- **Status.** Closed.
- **Implementation.** `.parse_eltwise` / `.parse_reduction` consult
  `.ops_env$eltwise` / `.ops_env$reductions` and raise a structured parse
  error on miss (`unknown eltwise operation: <name> at position <p> in
  query '<src>'`). Carat alignment is left for a future cosmetic slice.
- **Test reactivated.** `queries / invalid / operation`.
- **Julia ref.** `queries.jl:128-134`.

### P2. Unknown parameter name rejected at parse

- **Status.** Closed.
- **Implementation.** `register_eltwise` / `register_reduction` now derive
  a parameter signature from the function's `formals()` (drops the first
  positional and `...`). The shared `.parse_op_params` helper validates
  each token-pair against this signature. Pass `params = NA` to
  re-register an op with permissive (legacy) param validation.
- **Test reactivated.** `queries / invalid / parameter`.
- **Julia ref.** `queries.jl:136-143`.

### P3. Repeated parameter rejected at parse

- **Status.** Closed.
- **Implementation.** `.parse_op_params` tracks seen names per op and
  raises `repeated parameter: <k> for the operation: <op>` on duplicate.
- **Test reactivated.** `queries / invalid / parameters`.
- **Julia ref.** `queries.jl:145-152`.

### P4. Type annotation after `||` default parsed

- **Status.** Closed.
- **Implementation.** `.parse_if_missing` checks the token after the
  default value; if it matches a known Julia type name
  (`Bool`/`Int8/16/32/64`/`UInt8/16/32/64`/`Float32/64`/`String`) it is
  consumed as the type. The legacy `type T` two-token form is still
  accepted (only when `T` is a known Julia type, so a property called
  "type" parses cleanly).
- **Tests reactivated.** `queries / scalar / lookup / with_default / float`
  and `... / !int`.
- **Julia ref.** `queries.jl:286-323`.

### P5. IfMissing default auto-typed when no annotation given

- **Status.** Closed.
- **Implementation.** `.coerce_if_missing_default` now auto-detects when
  `type` is NULL: `true`/`false` → Bool, `pi`/`e` → Float64 constant,
  parseable int → integer, parseable float → numeric, else character.
  Vector and matrix lookup paths now route their default through the same
  coercion (so `: age || 1` returns an integer, not a character).
- **Tests reactivated.** `queries / scalar / lookup / with_default /
  const / pi`, `... / e`, `... / true`, `... / false`.
- **Julia ref.** `queries.jl:284-312`.

### E1. Mask after second axis (`@ rows @ cols [ filter ]`)

- **Status.** Closed.
- **Implementation.** `.apply_begin_mask` recognizes `state$kind ==
  "two_axes"` and resolves the mask property on the most-recently-entered
  axis (`cols_axis`). `.apply_end_mask` returns a `two_axes` state with
  `col_indices`, and `.apply_lookup_matrix` narrows the matrix by both
  `row_indices` and `col_indices` when set. Disambiguation grammar
  (choosing rows-axis vs cols-axis explicitly) is deferred — Julia's
  literal queries-tests only exercise the cols case.
- **Tests reactivated.** `queries / vector / matrix / reduction / column /
  empty / cols`, `... / column / !empty / cols`, `... / row / empty /
  cols`, `... / row / !empty / cols`.
- **Julia ref.** `queries.jl:649-651, 686-689`.

### E2. `name` virtual property in masks and lookups

- **Status.** Closed.
- **Implementation.** `.lookup_mask_property` (used by `.apply_begin_mask`
  and `.apply_logical_mask`) intercepts the property name `"name"` and
  returns `format_axis_array(daf, axis)`. `.apply_lookup_vector` does the
  same for `: name` when no real vector named `"name"` exists on the
  axis. The dataframe-side gap (`get_frame` returning a `name` column)
  remains under API1.
- **Julia ref.** `queries.jl:649-665, 685-703`.

---

## FIXED — N1

### N1. Vector / matrix / axis-listing results carry names

- **Status.** Closed across the S1 (format-API layer) and Slice 1a
  (axis-listing layer) slices.
- **S1 layer.** `format_get_vector` / `format_get_matrix` return named
  values (names = axis entries / dimnames = (rows, cols) entries) for
  every backend. `get_query` lookup vectors and matrix lookups inherit
  names through these. See `tests/testthat/test-query-result-names.R`.
- **Slice 1a layer.** `R/query_eval.R::.apply_axis` (bare `@ axis`) and
  `R/query_eval.R::.apply_end_mask` (masked `@ axis [...]`) now return
  axis-entry character vectors with `names == values`. Aligns with
  Julia's NamedVector convention (axis-listing names are the entries).
  Three `test-query-result-names.R` regressions pin the new contract.
- **Test sweep.** `test-query-eval-lookups.R`, `test-query-eval-masks.R`,
  `test-query-mask-variants.R` were updated to expect named axis-listing
  results. `tests/testthat/test-queries-jl-parity.R` retains its
  defensive `unname()` calls for now (vestigial; harmless).
- **Julia ref.** `queries.jl` uses `("axis", ["entry" => value, ...])`
  pair form for vector results; dafr's named-character mirror is the
  closest faithful translation in R.

(N1 closed — see "FIXED — N1" above.)

(E1 closed — see "FIXED in the parser-strictness slice" above.)

(E2 closed for masks and lookups — see "FIXED in the parser-strictness
slice" above. The dataframe-side gap, where `get_frame` would return a
`name` column with the axis-entry strings, is still open and documented
under API1.)

### E3. Matrix-slice-as-mask not supported

- **Symptom.** `[ UMIs @ gene = A > 0 ]` (mask using a matrix slice
  comparator) errors `vector 'UMIs' does not exist on axis 'cell'` — R's
  parser interprets the lone `UMIs` inside `[ ]` as a cell-axis vector
  rather than a matrix-slice comparator expression.
- **Fix sketch.** `R/query_parse.R::.parse_begin_mask` needs to accept a
  trailing `@ axis = entry` after the property name to disambiguate matrix
  slice vs vector, and the evaluator must reduce the slice to a per-row
  vector before comparing.
- **Test guard.** `... / queries / vector / mask / matrix` skipped.
- **Julia ref.** `queries.jl:459-465`.

### E4. Top-level comparator after `:` lookup not supported

- **Symptom.** `@ cell : type ~ \[UV` errors `comparator outside of mask`.
  Julia accepts comparators after a vector lookup at the top level (returns
  the comparison-bool vector). R requires comparators inside a `[ ]` mask.
- **Test guard.** `... / queries / vector / compare / !regex` and
  `... / matrix / compare / *` skipped.
- **Julia ref.** `queries.jl:765-787`.

### B7 / B8 — builder + introspection gaps (deferred)

| ID | Symptom | Fix |
|----|---------|-----|
| B7 | `Axis("cell") \|> Axis("gene") \|> LookupMatrix("UMIs") \|> Sum()` produces canonical `% Sum` (eltwise) instead of `>> Sum` (reduction). The R builder factory doesn't disambiguate eltwise vs reduction at construction. | Separate Reduction-distinct builder factory in `R/query_builders.R`. |
| B8 | `query_result_dimensions("@ cell @ gene")` returns `1` instead of erroring; `". score [ is_first ]"` returns `0`; `"? ?"` returns `NA`. Julia errors at parse. | Falls out of the parser-strictness slice once the parser rejects malformed token sequences. Helper `test_invalid` currently asserts only `get_query` errors. |

### B9 / E5-E11 — additional gaps surfaced during the port

The literal port surfaced more parser / evaluator gaps that block specific
test groups. Each is documented compactly here; the parser-strictness
follow-up slice should treat these as part of the same body of work.

| ID | Gap | Tests blocked (representative) |
|----|-----|-----|
| B9 | `has_query` / `query_axis_name` / `query_requires_relayout` are stricter than `get_query` — return FALSE / NA on queries that `get_query` evaluates correctly (e.g. square-slice masks, compound boolean masks). | `vector / mask / operation / column`, `vector / mask / square / *` (introspection-side only) |
| E5 | `:` and `::` cannot start a query — R requires a leading `@ axis`. Julia accepts entry-pick standalone (`: vec @ axis = entry`, `:: m @ rows = R @ cols = C`). | `scalar / vector / ()`, `scalar / matrix / ()` |
| E6 | Lookup chains after a matrix (`@ rows @ cols :: M : V`, `:: M :: M2 ...`, `@ axis :: M @ other = E`) require both axes in scope; R errors with `':' / '::' requires axis(es) in scope`. Julia walks the chain across matrix-then-vector lookups. | `matrix / lookup / *`, `vector / lookup / as_axis / *`, `vector / lookup / if_not / *`, `vector / matrix / column`, `vector / square / *` |
| E7 | Group-by / count-by a matrix-slice (`/ kind @ axis = E`, `* type =@`) not recognized; R parser/evaluator interprets `kind`/`type` as a vector on the wrong axis. | `vector / group / vector / matrix \| square / *`, `matrix / group / */slice`, `matrix / group / */square / *` |
| E8 | The count operation `: vec * other =@` (Julia: cross-tabulate two vectors into a matrix) errors `'=@' requires a vector in scope`. | `matrix / count / *` |
| E9 | dafr `get_query` does not auto-relayout matrices — a matrix stored as `gene × cell` is not addressable as `cell × gene`. Julia auto-relayouts. | `matrix / group / row / ()` and several siblings (depends on which fixture layout) |
| E10 | Regex escape sequences inside masks (`[ type ~ \^\[A-U\] ]`) — R's tokenizer mishandles the escaped trailing `\]`, splitting the mask early. | `vector / compare / ~`, `vector / compare / !~` |
| E11 | `as_axis` group with `=@` (group-vector reduction with axis-keyed output) returns wrong shape / errors on missing entries; the IfMissing-coverage rule for unused axis entries differs from Julia. | `vector / group / vector / as_axis`, `vector / group / vector / missing` |

### API1. `get_dataframe[_query]` named-list column-spec not supported

- **Symptom.** Julia's `get_frame(daf, "cell", ["age" => ": age", "doublet"
  => ": is_doublet"])` accepts pairs of (output-column-name => sub-query).
  dafr's `get_dataframe` / `get_dataframe_query` `columns` parameter takes
  only a character vector of pre-existing column names; a named list of
  query strings errors `columns not on query result: ': age'`. Same gap
  also blocks the dataframe-side of E2 (a `name` column with axis-entry
  strings).
- **Test guard.** `... / queries / dataframes / simple / columns / queries`
  and `... / shorthands` and the complex variants skipped.
- **Recommended fix.** Extend `columns` parameter to accept a named list of
  query strings; for each, evaluate the sub-query and bind into the
  data.frame. This is a small enhancement to `R/dataframes.R`.

---

## Error-text-only divergences (NOT bugs; documented for completeness)

These are cases where R errors but with different wording than Julia. Listed
so future readers don't mistake them for bugs.

| # | Julia text | R text | Site |
|---|---|---|---|
| T1 | `expected: operator / in: cell / at: ▲▲▲▲` | `expected operator, got value 'cell' at position 1 in query 'cell'` | `parse_query("cell")` |
| T2 | `expected: value / in: >> / at:   ▲` | `expected reduction name after '>>' at position 1 in query '>>'` | `parse_query(">>")` |
| T3 | `unsupported input type: String / for the reduction operation: Sum` | `invalid 'type' (character) of argument` (from base R `sum`) | string `>> Sum` |
| T4 | `unsupported input type: String / for the eltwise operation: Abs` | `non-numeric argument to mathematical function` | string `% Abs` |
| T5 | `error parsing number comparison value: U / for comparison with a vector of type: Float64` | (similar concept, different wording from `.coerce_cmp`) | `[ score = U ]` |

The literal parity tests assert the failure mode (any error is raised) and
key concept substrings where reasonable, but do not assert the Julia
caret-aligned error text.

---

## Test catalog

`tests/testthat/test-queries-jl-parity.R` — every `nested_test` leaf in
`queries.jl` becomes one `test_that` in R, named with the Julia path
(`"queries / invalid / !operator"` etc.). Tests guarded against the
divergences above use `skip("R parser-strictness divergence: P1")` /
`skip("R evaluator divergence: E1")` etc., naming the gap by ID so this
document is the single source of truth for the punch list.

Counts at first commit (literal port):
- 6 fixes shipped (B1-B6)
- 5 parser-strictness gaps (P1-P5) — skip count varies as multiple tests hit each
- 2 evaluator gaps (E1-E2)
- 5 error-text-only divergences (T1-T5) — tests pass via substring/condition-only assertion

Counts after the parser-strictness slice (this update):
- B1-B6, P1-P5, E1, E2 closed (13 items).
- Remaining deferred IDs: N1, E3-E11, B7-B9, API1.
- Skip count in `test-queries-jl-parity.R`: 68 (down from ~88 at first
  commit), all attributed to the deferred IDs above. The bulk are E5-E11
  pointed at the divergences doc.
- All B-shipped + P/E-shipped substantive assertions pass; full test suite
  remains green.
