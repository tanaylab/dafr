# Slice exit: reorder.jl literal parity port

**Date:** 2026-05-07
**Branch:** `dev` (committed directly, matching prior parity-slice practice)
**Predecessor:** `dev/notes/2026-05-06-slice-concat-jl-parity-exit.md`
(concat.jl parity port; the second of Slice B's three files.)

## Result

`FAIL 0 | WARN 1 | SKIP 19 | PASS 4790` on the full suite (`cd tests &&
NOT_CRAN=true Rscript testthat.R`). Delta vs the post-concat baseline
(4690 PASS / 12 SKIP): **+100 PASS, +7 SKIP** from the new parity
file. The 1 warning is the same pre-existing scran SVD warning. No
regressions.

The new file `tests/testthat/test-reorder-jl-parity.R` has 33
`test_that` blocks — h5df / zarr_reorder / multiple_writers groups are
collapsed into one skip-stub each (the divergence is backend-wide, not
per-test).

## What changed

### Inline behavior fixes (2)

1. **R0 — `reorder_axes` now errors on missing axes.**
   `R/reorder_plan.R::.build_reorder_plan` previously had an explicit
   `if (!format_has_axis(daf, axis)) next` (silent skip). Replaced with
   a `stop("axis: %s does not exist in the daf data: %s", ...)`. The
   pre-existing `test-reorder-plan.R` test that pinned the silent-skip
   behavior was rewritten to assert the new error contract.

2. **R7 — `reset_reorder_axes` now returns a Bool.** Previously the
   wrapper returned `invisible(daf)`, throwing away the format method's
   TRUE/FALSE. Now: `invisible(isTRUE(format_reset_reorder(...)))`.
   `MemoryDaf::format_reset_reorder` updated to return
   `invisible(FALSE)` explicitly (was `invisible()` = NULL).
   `FilesDaf::format_reset_reorder` already returned the right
   `invisible(TRUE/FALSE)` so untouched. Existing R-side callers used
   `reset_reorder_axes` for side effect only (`expect_silent(...)`),
   so this is invisible to them.

### Documented divergences (5 IDs, 7 skips)

| ID | Gap | Notes |
|----|-----|-------|
| R2 | `is_leaf` class-level dispatch (Julia) vs. instance-only (dafr) | Replaceable with `inherits(.)` at the call site; low priority |
| R3 | dafr does not have an h5df backend | Out of scope — not on the roadmap |
| R4 | `zarr_daf` does not implement reorder | Implementable but not scheduled |
| R5 | `reorder_axes` does not accept a list of writers | ~50-line lift; would surface the cross-writer entry-mismatch error path that's currently inaccessible in dafr |
| R6 | `memory_daf` reorder is in-place not atomic | dafr's design comment justifies this on process-crash terms, but it doesn't extend to user-facing exceptions; a snapshot-on-entry pattern would close the gap |

R6 is worth flagging. The dafr design comment at `R/memory_daf.R:427-428`
explicitly says: "No on-disk crash recovery needed; if R crashes the
in-memory store is gone anyway." That justification doesn't cover an
in-process exception (a user-thrown `stop()` mid-reorder) leaving the
daf partially-permuted. Lifting to atomic semantics is a 30-50 line
fix; documenting and skipping for now.

## Files touched

- `R/reorder.R` — R7 fix (5-line rewrite of `reset_reorder_axes`).
- `R/reorder_plan.R` — R0 fix (silent-skip → stop).
- `R/memory_daf.R` — R7 follow-on (`format_reset_reorder` returns
  `invisible(FALSE)`).
- `tests/testthat/test-reorder-plan.R` — pre-existing silent-skip test
  rewritten to assert the new error contract.
- `tests/testthat/test-reorder-jl-parity.R` — new, ~370 lines.
- `dev/notes/2026-05-07-reorder-jl-parity-divergences.md` — new.
- `dev/notes/2026-05-07-slice-reorder-jl-parity-exit.md` — this file.

## Next slice

Per Slice B, one file remains: `chains.jl` (537 lines, 252 @tests). The
biggest of the three (and the one that absorbs the wrapper-mode
read-only enforcement that Slice A's misread originally targeted).
Empty-chain / unnamed-chain semantics + `ReadOnlyChainDaf` enforcement.

Ready to ship.
