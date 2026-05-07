# Slice exit: chains.jl literal parity port — closes Slice B

**Date:** 2026-05-07
**Branch:** `dev` (committed directly, matching prior parity-slice practice)
**Predecessor:** `dev/notes/2026-05-07-slice-reorder-jl-parity-exit.md`
(reorder.jl parity port; the second of Slice B's three files.)

## Result

`FAIL 0 | WARN 1 | SKIP 31 | PASS 4972` on the full suite (`cd tests &&
NOT_CRAN=true Rscript testthat.R`). Delta vs the post-reorder baseline
(4790 PASS / 19 SKIP): **+182 PASS, +12 SKIP** from the new parity
file. The 1 warning is the same pre-existing scran SVD warning. No
regressions.

The new file `tests/testthat/test-chains-jl-parity.R` has 49
`test_that` blocks. The `access` group's read/write parameterization
(Julia's `for (name, type_name, chain) in [...]`) is unrolled as 32
test_thats sharing 14 `.assert_access_*` helpers.

## What changed

### Inline behavior fix (1, but with broad impact)

**C0 — Chain version counters now propagate from underlying sources.**
Previously, `ReadOnlyChainDaf` and `WriteChainDaf` each had their own
private `vector_version_counter` / `axis_version_counter` /
`matrix_version_counter` env, but those envs were never bumped when an
underlying source's vector/axis/matrix was mutated independently. Two
visible failure modes:

1. **Counter reads stale.** `vector_version_counter(chain, axis,
   name)` returned 0 even after `set_vector(source, axis, name, ...)`
   had bumped the source's counter to 1. The
   `chains/access/{read,write}/vector/{first,second}` tests assert the
   chain's counter increments after a source-side mutation; all four
   failed.

2. **Cache invalidation broke after chain writes.** The cache stamp
   computed via `vector_stamp(daf, ...)` read from the chain's stale
   env. After `set_vector(chain, axis, name, ...)` (which routes the
   write to the chain's writer source), reading via the chain hit the
   cache from before the write and returned the previous source's
   value instead of the writer's new value. The `chains/write/vector/
   override`, `chains/write/vector/change`, `chains/write/matrix/
   override`, `chains/write/matrix/change` tests all caught this.

**Fix:** In `R/cache.R`, route `axis_stamp`, `vector_stamp`,
`matrix_stamp`, `axis_version_counter`, `vector_version_counter`,
`matrix_version_counter` through a `.is_chain(daf)` check. When the
daf is a chain, sum the per-source counters / stamps. Recursion
naturally handles chain-of-chain. The chain's own counter env is now
unused but kept on the class (removing it would be its own slice of
S7 prop changes for no behavior delta).

### Documented divergences (4 IDs, 12 skips)

| ID | Gap | Notes |
|----|-----|-------|
| C1 | Singleton `chain_reader([d])` / `chain_writer([d])` always wraps; Julia returns `d` itself | ~10 lines per constructor; pleasant idempotence improvement |
| C2 | `description(...; deep = TRUE)` not supported (no `deep` param at all) | ~30-line lift; deep-mode pretty-printer needs to walk chain members |
| C3 | No `empty_dense_*` / `empty_sparse_*` builder API | Substantial; R has no obvious zero-copy idiom anyway, the call site can use `set_vector` directly |
| C5 | `complete_path` errors on memory-backed chains; Julia returns `nothing` | ~5 lines; relaxed inline in the parity test rather than skipping |

C0's fix has broad impact: every cache-using chain operation now sees
correct invalidation. This was a real bug that the existing R-side
chain tests didn't surface because they didn't exercise the
after-mutation read-through-chain path.

## Slice B closes

Slice B (concat.jl + reorder.jl + chains.jl) is now done. Cumulative:

| File | Bugs fixed | Divergences open | New tests |
|------|-----------:|-----------------:|----------:|
| concat.jl | 2 (B1, B2) | 4 (M1, M2, M4, M5) | 64 PASS / 6 SKIP |
| reorder.jl | 2 (R0, R7) | 5 (R2, R3, R4, R5, R6) | 100 PASS / 7 SKIP |
| chains.jl | 1 (C0, broad) | 4 (C1, C2, C3, C5) | 182 PASS / 12 SKIP |
| **Slice B total** | **5** | **13** | **+346 PASS / +25 SKIP** |

Full suite: `4626 PASS` (pre-Slice-B) → `4972 PASS` (post-Slice-B).

## Files touched

- `R/cache.R` — C0 fix (chain-aware version counters / stamps).
- `tests/testthat/test-chains-jl-parity.R` — new, ~625 lines.
- `dev/notes/2026-05-07-chains-jl-parity-divergences.md` — new.
- `dev/notes/2026-05-07-slice-chains-jl-parity-exit.md` — this file.

## Next slice

Slice C (`views.jl` + `contracts.jl`, 2293 lines combined). Wrapper-API
stress tests. Per the kickoff doc, the suggestion is to split into:

- C1: `views.jl` (654 lines, 126 @tests) + `contracts.jl` add-only
  (~half of contracts.jl).
- C2: `contracts.jl` verify + as-reader (other half).

Not started.

Ready to ship Slice B to `main`.
