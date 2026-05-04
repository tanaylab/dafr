# Slice 3 — Julia parity architectural (E6, E9) — Exit note

**Date:** 2026-05-04
**Branch:** `slice-julia-parity-3` off `dev` post-Slice-2 (`05e29aa`).
**Predecessor:** `slice-julia-parity-2-exit.md`.
**Successor:** None — every E-class divergence in the audit is now
closed except E11 (kernel-level type-strictness, out of scope).

## Scope delivered

- **E6 (implicit AsAxis fallback).** `.apply_chained_lookup_vector` now
  walks the property name backward across `.` separators when the
  property name doesn't match an axis directly. So `type.manual` →
  `type`, `kind.left.right` → `kind.left`. Mirrors Julia's
  `ensure_vector_is_axis`. Two un-skipped tests:
  `vector / lookup / as_axis / implicit` and `... / explicit`.
- **E6 (matrix-column slice auto-relayout).**
  `.apply_matrix_column_by_axis` now auto-transposes when the matrix
  is stored as `(cols, rows)` instead of the queried `(rows, cols)` —
  same semantics as `.apply_lookup_matrix`'s existing relayout. One
  un-skipped test: `vector / matrix / column`.
- **E9 (auto-relayout).** Already worked at every test site; no skip
  remained for this class.

## Bonus: three T-class wins

While in the file, three previously-T-class skips turned out to be
tractable with small error-handling tweaks:

- **`>| Sum` / `>- Sum` on character matrix.** `.apply_reduction` now
  type-checks `is.character(state$value)` before dispatching to fast
  or slow paths and raises a clean `non-numeric input: cannot apply
  <Reduction> reduction to a character matrix` error. Replaces the
  base R `'x' must be numeric` that leaked through. Two un-skipped
  tests.
- **`?? foo : phase` IfNot sentinel coercion.**
  `.apply_chained_lookup_vector` now wraps the
  `methods::as(sentinel, target_class)` coercion in
  `withCallingHandlers` and converts the `NAs introduced by coercion`
  warning into a `cannot parse IfNot sentinel <foo> as <type>` error.
  One un-skipped test.

## Numbers

- **Pre Slice 3:** `FAIL 0 | WARN 1 | SKIP 12 | PASS 4610`.
- **Post Slice 3:** `FAIL 0 | WARN 1 | SKIP 6 | PASS 4619` (+9 net).
- **End-to-end (Slices 1a → 3):** -66 skips, +130 passes vs the
  pre-N1 baseline.

## Files touched

- `R/query_eval.R`:
  - `.apply_chained_lookup_vector` — base-axis fallback for
    `type.manual` → `type`; sentinel-coercion handler.
  - `.apply_matrix_column_by_axis` — auto-relayout via transpose.
  - `.apply_reduction` — type-check character input before dispatch.
- `tests/testthat/test-queries-jl-parity.R` — 5 un-skipped tests.
- `dev/notes/2026-05-03-queries-jl-parity-divergences.md` — final
  status table.
- `NEWS.md` — Slice 3 section.

## Final state

Every E-class evaluator divergence in the original audit is closed:
**E1, E2, E3, E4, E5, E6, E7, E8, E9, E10**. Plus B1-B7, B9, P1-P5,
N1, API1. Only E11 remains (and it's a real type-system divergence,
not a logic gap — R's reduction kernels promote int→double
unconditionally, dropping the type signal Julia uses for InexactError).

The 2 remaining skips in `test-queries-jl-parity.R` are the two E11
`~missing` tests under sharp T-class skip messages. The 4 other
suite-wide skips are unrelated (mmap-zip-recovery on CRAN, etc.).
