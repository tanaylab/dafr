# Slice 1a — Julia parity N1 (named axis-listing) — Exit note

**Date:** 2026-05-04
**Branch:** `slice-julia-parity-N1` off `dev` tip `eb2721a`.
**Predecessor:** `slice-s1-on-main-and-realignment-exit.md`
  (S1 names-everywhere on `format_get_*`).
**Successor:** Slice 1b — E4/E5/E10/B7/B9/API1 quick-win parity fixes.

## Scope delivered

Closes N1 — the last remaining gap in the "named query results" contract.
S1 fixed `format_get_*` (lookups already named); this slice closes the
axis-listing paths that go straight to `state$value` without a lookup.

Two sites in `R/query_eval.R`:

- **`.apply_axis`** (bare `@ axis`): the entry-character vector now carries
  `names = entries`.
- **`.apply_end_mask`** (masked `@ axis [ ... ]`): surviving entries carry
  `names = surviving_entries`.

Result: `get_query(d, "@ cell")` returns
`c(c1 = "c1", c2 = "c2", c3 = "c3")` (was unnamed character vector).
`get_query(d, "@ donor [ age > 60 ]")` returns `c(d3 = "d3", d4 = "d4")`.
This matches Julia's `NamedVector` axis-listing convention where the
names *are* the values.

## Tests

- **New** in `tests/testthat/test-query-result-names.R`:
  - `bare axis listing returns named character vector`
  - `masked axis listing returns named character vector`
  - `masked-out-empty axis listing returns named character(0)`
- **Flipped** to expect named axis-listing:
  - `test-query-eval-lookups.R` (1 assertion)
  - `test-query-eval-masks.R` (7 assertions)
  - `test-query-mask-variants.R` (15 assertions)
- **`test-queries-jl-parity.R`** unname() workarounds left in place
  (vestigial; harmless; documented in updated header comment).

## Numbers

- **Pre:** `FAIL 0 | WARN 1 | SKIP 72 | PASS 4489`
- **Post:** `FAIL 0 | WARN 1 | SKIP 72 | PASS 4495` (+6 net: 3 new
  assertions in `test-query-result-names.R`; 23 flipped existing
  assertions remain at PASS 1).

## Per-phase commits (on `slice-julia-parity-N1`)

Single commit landing the fix + test sweep + docs in one shot. (TDD red →
green flow internally; squashed because the sites are tightly coupled and
the test sweep is a mechanical follow-on.)

## Follow-ups handed to Slice 1b

- **E4** top-level comparator after `:` / `::`
- **E5** `:` / `::` standalone (start a query)
- **E10** regex escape sequences in masks
- **B7** `Sum()` builder produces canonical `>> Sum` not `% Sum`
- **B9** `has_query` / `query_axis_name` / `query_requires_relayout`
  introspection strictness
- **API1** named-list column-spec for `get_frame` / `get_dataframe`

Slice 2 still owns E3 / E7 / E8 / E11; Slice 3 still owns E6 / E9.

## Ship plan

`ship.sh` from `dev/skills/dafr-ship/ship.sh` after merging this slice
into `dev`. Main ↔ dev tree-equivalence outside `dev/` + `AGENTS.md`
preserved.
