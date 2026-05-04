# Slice 2 — Julia parity medium evaluator (E3 / E7 / E8 / E11) — Exit note

**Date:** 2026-05-04
**Branch:** `slice-julia-parity-2` off `dev` post-Slice-1b (`46f3785`).
**Predecessor:** `slice-julia-parity-1b-exit.md`.
**Successor:** Slice 3 — E6 / E9 (architectural).

## Scope delivered

Three of the four IDs in scope already worked off the cumulative B / P /
E1-E2 / N1 / Slice-1b fixes. The fourth (E11) is a real divergence but
sits below the level dafr can address without rewriting reduction
kernels — reclassified as T-class.

- **E3** (matrix-slice-as-mask, `[ UMIs @ gene = A > 0 ]`) — un-skipped.
- **E7** (group/count-by a matrix-slice) — no remaining skip; the only
  E7-marked test class (`vector / group / vector / matrix`) was already
  passing.
- **E8** (cross-tabulate `: vec * other =@`) — un-skipped. The
  failure was actually a port-bug: the dafr test asserted `sum=2`,
  but the Julia reference (`queries.jl:1134-1144`) lists six
  `(width, type) => count` entries summing to **3** (1+1+0+1+0+0).
  Corrected the assertion.
- **E11** (`as_axis` group with `=@` IfMissing-coverage) **reclassified
  to T-class.** Not closeable here. Investigated thoroughly: the
  `~missing` test asserts `Int|0\\.5|InexactError|coerce|convert|integer`,
  i.e. it expects an error when the IfMissing default (`0.5`) is
  non-integer for an integer-typed matrix. R's reduction kernels
  (`kernel_grouped_reduce_*` etc.) promote integer matrices to `double`
  during `Sum` — by the time the IfMissing fill site runs, the result
  is already `is.numeric() == TRUE` and `is.integer() == FALSE`, so a
  type-strictness check there can't distinguish "matrix was originally
  integer" from "matrix was always double". Closing E11 properly would
  mean threading an `expected_output_type` flag through the reduction
  pipeline (kernel + state machine + IfMissing fill) — large surface
  area, low value (R is permissive about numeric coercion by design).
  Skip messages for the two `~missing` E11 tests now read T-class.

## Files touched

- `tests/testthat/test-queries-jl-parity.R` — un-skipped E3 + E8
  (with corrected sum); reclassified 2 E11 skips to T-class.
- `dev/notes/2026-05-03-queries-jl-parity-divergences.md` — status
  table updated.
- `NEWS.md` — Slice 2 section.

No code changes in `R/`. The Slice 2 win is mostly diligence: confirming
that prior cumulative fixes had already closed the items, fixing one
test bug, and pinning down E11 as out-of-scope T-class.

## Numbers

- **Pre Slice 2:** `FAIL 0 | WARN 1 | SKIP 14 | PASS 4603`.
- **Post Slice 2:** `FAIL 0 | WARN 1 | SKIP 12 | PASS 4610` (+7 net).

## Follow-ups handed forward

- **Slice 3** (E6, E9): vector-by-vector / matrix-then-vector chains;
  auto-relayout. These are the last two semantic gaps.
- E11 type-strictness — out of scope; would require kernel-level
  type-tracking. Might revisit if/when a reduction-kernel rewrite is
  done for other reasons.
