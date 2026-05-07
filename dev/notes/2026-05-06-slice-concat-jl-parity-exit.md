# Slice exit: concat.jl literal parity port

**Date:** 2026-05-06
**Branch:** `dev` (committed directly, matching prior parity-slice practice)
**Predecessor:** `dev/notes/2026-05-05-jl-parity-port-rest-kickoff.md`
(Slice A reframed and dropped; concat.jl was Slice B's first file.)

## Result

`FAIL 0 | WARN 1 | SKIP 12 | PASS 4690` on the full suite (`cd tests &&
NOT_CRAN=true Rscript testthat.R`). Delta vs the 4626 baseline: **+64
PASS, +6 SKIP** from the new parity file. The 1 warning is a pre-existing
scran SVD warning unrelated to concat. No regressions in any of the
~180 other test files.

The new file `tests/testthat/test-concat-jl-parity.R` has 33 `test_that`
blocks (≈42 nested_test leaves in Julia minus a handful folded together
where dafr's API doesn't distinguish sub-paths). 27 pass, 6 skip with
divergence-IDs keyed to
`dev/notes/2026-05-06-concat-jl-parity-divergences.md`.

## What changed

### Inline behavior fixes (2)

1. **B1 — `MERGE_COLLECT_AXIS` for vector now honors `empty`.**
   `R/concat.R::.concat_merge_vector` previously left columns for
   missing-source entries as `NA`, ignoring the user's `empty=` map.
   Plumbed `empty` from `concatenate()` through `.concat_merge()` into
   `.concat_merge_vector()`; the COLLECT_AXIS branch now consults
   `empty[[axis|name]]` for fills and raises the same "no empty value"
   error as the per-axis vector path when neither source nor empty
   provides a value.

2. **B2 — `MERGE_LAST_VALUE` for 3-part matrix keys now actually fires.**
   Previously a silent no-op: a key like `"gene|gene|outgoing_edges" =
   MERGE_LAST_VALUE` made it through `.concat_merge`'s `length(parts) ==
   3L` branch but dropped through without doing anything. Added
   `.concat_merge_matrix()` which iterates sources in reverse and
   stamps the last source's matrix on the destination. The
   COLLECT_AXIS branch (which errors with "would create a 3D tensor")
   is unchanged.

Both fixes are covered by the parity tests `concat / merge / vector /
collect / dense / empty / {zero, !zero}` (B1) and `concat / merge /
matrix / {square, rectangle} / last` (B2). No new tests added to
`test-concat.R` — the parity file is the regression guard.

### Documented divergences (4 IDs, 6 skips)

| ID | Gap | Fix path |
|----|-----|----------|
| M1 | `concatenate(merge=)` doesn't expand `ALL_SCALARS / ALL_VECTORS / ALL_MATRICES` wildcards | ~30-line addition in `.concat_merge`; future feature lift |
| M2 | Vector COLLECT_AXIS path always allocates dense | Pairs with M5 |
| M4 | `prefixed=` is gated by per-axis `prefix` flag in dafr; Julia treats it as override regardless | Behavioral question — pin contract before changing gate |
| M5 | `memory_daf` rejects `Matrix::sparseVector` (`.validate_vector_value` atomic gate); `files_daf` accepts | Either accept-and-densify or accept-and-store-sparse |

### Kickoff doc updated

`dev/notes/2026-05-05-jl-parity-port-rest-kickoff.md` records that Slice
A (`read_only.jl`) was dropped on inspection — that file is array-
primitives only (`is_read_only_array`, `read_only_array`, `copy_array`,
`brief()` strings), not wrapper-mode enforcement, and R has no
counterparts for the primitives. Wrapper-mode read-only behaviors live
in chains.jl (Slice B), views.jl/contracts.jl (Slice C), and data.jl
(Slice F) and will be ported there. R-side read-only surface is already
covered by `tests/testthat/test-altrep-*.R`.

## Files touched

- `R/concat.R` — B1 + B2 fixes (~50 added/changed lines).
- `tests/testthat/test-concat-jl-parity.R` — new, 619 lines.
- `dev/notes/2026-05-05-jl-parity-port-rest-kickoff.md` — Slice A
  reframe.
- `dev/notes/2026-05-06-concat-jl-parity-divergences.md` — new.
- `dev/notes/2026-05-06-slice-concat-jl-parity-exit.md` — this file.

## Next slice

Per the kickoff's Slice B, next files in this body of work:

- `reorder.jl` (500 lines, 130 @tests) — reorder × wrapper combinations.
- `chains.jl` (537 lines, 252 @tests) — empty-chain / unnamed-chain
  semantics, `ReadOnlyChainDaf` (which absorbs the read-only-wrapper
  enforcement that Slice A's misread originally targeted).

Ready to ship to `main`.
