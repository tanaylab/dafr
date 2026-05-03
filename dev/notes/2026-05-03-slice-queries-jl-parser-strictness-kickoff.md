# Slice — queries.jl parser-strictness — Kickoff breadcrumb

**Date:** 2026-05-03
**Branch hint:** `slice-queries-jl-parser-strictness` (number TBD; 19 / 20 are
unused in the slice sequence, slice-23+ also free).
**Predecessor:** literal queries.jl parity port on `dev` (this date), which
catalogued every divergence in
`dev/notes/2026-05-03-queries-jl-parity-divergences.md` and shipped six
evaluator behavior fixes (B1-B6). The follow-up is unblocking the tests that
stayed `skip()`-guarded against parser/evaluator-level structural gaps.

## Motivation

The literal port revealed a **structural difference**: DAF.jl's parser is
registry-aware (validates op + parameter names + types at parse time), R's is
a pure tokens→AST translator. The same gap explains five of the seven
deferred items. Closing it lets us delete the `skip()` guards from the parity
tests and have them actually exercise R's parser the way Julia exercises
DAF.jl's — at parse time, with caret-aligned context.

The two evaluator-side gaps (E1, E2) are independent of the parser work but
unblock the same parity tests, so they belong in the same slice.

## Scope (from the divergences doc, IDs preserved)

### Parser-strictness (P1-P5)

| ID | Gap | Fix shape | Est. lines |
|----|-----|-----------|-----:|
| P1 | Unknown eltwise/reduction op name not rejected at parse | Look up name in `get_eltwise` / `get_reduction` registry from `.parse_eltwise` / `.parse_reduction`; raise structured parse error on miss. Need carat-aligned context helper. | ~50 |
| P2 | Unknown parameter name not rejected at parse | Each registered op needs a parameter signature (name → type). Extend op registration so parser can introspect. | ~80 |
| P3 | Repeated parameter not rejected at parse | Local: track seen-param-names per op in the parser loop; error on duplicate. | ~10 |
| P4 | Type annotation after `||` default not parsed (`Float64`, `Int32`, ...) | `.parse_if_missing` peeks past the value token; if next token matches a known type name, consume it. Validate value coerces. | ~30 |
| P5 | IfMissing default returns raw character; auto-typing absent | `.coerce_if_missing_default` with `type=NULL`: detect Bool / Int64 / Float64 / String from the literal. Add `pi` / `e` constants. | ~40 |

### Evaluator (E1-E2, N1)

| ID | Gap | Fix shape | Est. lines |
|----|-----|-----------|-----:|
| E1 | Mask after second axis (`@ rows @ cols [ filter ]`) | `.apply_begin_mask` extends to `state$kind == "two_axes"`; downstream LookupMatrix and reductions accept a mask-narrowed two_axes state. Disambiguation: mask filters cols_axis (most-recent). | ~60 |
| E2 | `name` virtual property | Intercept `name` in `.apply_lookup_vector` / `.apply_begin_mask` to return `format_axis_array(daf, axis)`. Treat as a per-axis virtual not stored in the format. | ~30 |
| N1 | Vector / matrix results returned without dimnames | Thread axis-entry names through `format_get_vector` / `format_get_matrix`; restore names after mask narrowing in `.apply_lookup_vector` / `.apply_lookup_matrix`. Existing R tests assert unnamed; they need to be updated alongside this fix. Risk: surface area touches every format adapter. | ~150 |

Total estimated payload: ~450 lines of R + minor parser refactors. N1 is the
largest item — could ship as its own sibling slice.

## What gets unblocked

In `tests/testthat/test-queries-jl-parity.R`, the `skip("R divergence: P*")`
and `skip("R divergence: E*")` guards are removed (test count varies because
several tests hit multiple gaps). Each gap closure should match Julia
verbatim on the substantive assertions; error-text-only tests (T1-T5 in the
divergences doc) stay on substring matching since R's error formatter isn't
caret-aligned.

A non-goal of this slice is to mimic Julia's exact carat-aligned error text;
that's an aesthetic match that doesn't change behavior. If desired, it can
be a separate later slice (probably needs a `cli` / `rlang` formatter
pass).

## Sequencing notes

- **P3 first** (~10 lines, no registry plumbing) — easiest win, removes one
  skip with minimal risk.
- **P1 + P2 together** — they share the registry-introspection plumbing.
  P2 also needs parameter signatures attached to each registered op, which
  is the larger surface change. Do P1 alone if P2's signature work blows up.
- **P4 + P5 together** — both touch `.parse_if_missing` /
  `.coerce_if_missing_default`. P4 doesn't fully work without P5 (you'd parse
  `Float64` correctly but the untyped `1.0` case still wrongly returns
  character).
- **E1 + E2** are independent of each other and of P*. E1 has the bigger
  surface (mask propagation through matrix-state).

## Validation

- `tests/testthat/test-queries-jl-parity.R` skip count drops to ≤ 5 (the
  text-only T-items stay).
- Existing tests remain green.
- One Julia-vs-R fixture roundtrip pass (`test-query-julia-compat.R`)
  re-confirms numerical parity on the example daf.

## Out of scope

- Carat-aligned error text formatter (cosmetic).
- Wider-axis mask grammar (e.g., disambiguation syntax to choose which of
  two_axes the mask filters) — defer until a real test demands it.
- Float-detection heuristic edge cases (NaN, Inf literals) — current Julia
  tests don't exercise; pin scope to what queries.jl uses.
