# Slice exit: views.jl literal parity port (Slice C, first half)

**Date:** 2026-05-07
**Branch:** `dev`
**Predecessor:** `dev/notes/2026-05-07-slice-chains-jl-parity-exit.md`
(closed Slice B). views.jl is Slice C's first half; contracts.jl is
the bigger second half (1639 lines, 459 @tests).

## Result

`FAIL 0 | WARN 1 | SKIP 48 | PASS 5005` on the full suite. Delta vs
post-chains baseline (4972 PASS / 31 SKIP): **+33 PASS, +17 SKIP**.
Same scran SVD warning, no regressions.

The new file `tests/testthat/test-views-jl-parity.R` has 50
`test_that` blocks. The tensor group is one collapsed skip-stub
covering 8 leaves.

## What changed

### Inline behavior fixes

None. The view layer's divergences are mostly missing features
(tensor support, `__axis__` placeholder, strict-include semantics,
wildcard validation) rather than bugs in existing behavior. Each
warrants its own scoped fix.

### Documented divergences (8 IDs, 17 skips)

| ID | Gap | Notes |
|----|-----|-------|
| V1 | No tensor support in viewer / description | ~150-200 lines; whole tensor group skipped (1 stub for 8 leaves) |
| V2 | Permissive wildcard query validation | ~30-line fix; Julia rejects `("*", "v") => "x"` shaped specs that dafr silently accepts |
| V3 | Data items add to default-all-visibility, not strict-include-list | Behavioral change — needs user discussion before flipping |
| V4 | `:: UMIs % Abs` query rejected by evaluator | ~10-line evaluator fix once root cause pinpointed |
| V5 | No `__axis__` placeholder in queries | ~20 lines per call site; pre-substitution before parse |
| V6 | `:: UMIs` (no leading axes) rejected by parser | Pairs with V5; view-matrix dispatch could prepend axes |
| V7 | View-scalar layer doesn't validate scalar-shape | ~5-line fix in ViewDaf format_get_scalar |
| C2 | `description(...; deep)` carryover from chains slice | Documented earlier |

V3 is the most user-facing — the default-all-visibility vs strict-
include-list semantic affects every viewer call. I'd flag it for
explicit user decision before changing.

## Files touched

- `tests/testthat/test-views-jl-parity.R` — new, ~390 lines.
- `dev/notes/2026-05-07-views-jl-parity-divergences.md` — new.
- `dev/notes/2026-05-07-slice-views-jl-parity-exit.md` — this file.

No `R/` files touched this slice.

## Slice C status

views.jl done. contracts.jl is the remaining half (1639 lines, 459
@tests — almost 3x larger than any single file ported so far). The
kickoff doc explicitly suggested splitting Slice C into C1 (views +
contract-add) and C2 (contract-verify + contract-as-reader). Given
contracts.jl's size, I'd recommend stopping here and re-evaluating
scope before starting it — the prior parity slices have been steady
state at ~3-5 inline fixes per file, but 459 @tests means the
divergence yield could plausibly be 20+ if contracts.jl's surface is
similarly Julia-flavored.

Ready to ship views.jl to `dev`.
