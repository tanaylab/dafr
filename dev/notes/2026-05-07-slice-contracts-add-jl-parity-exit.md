# Slice exit: contracts.jl / add subgroup parity port

**Date:** 2026-05-07
**Branch:** `dev`
**Predecessor:** `dev/notes/2026-05-07-slice-views-jl-parity-exit.md`

## Result

`FAIL 0 | WARN 1 | SKIP 49 | PASS 5024` on the full suite. Delta vs
post-views baseline (5005 PASS / 48 SKIP): **+19 PASS, +1 SKIP**.

The new file `tests/testthat/test-contracts-add-jl-parity.R` has 14
`test_that` blocks covering Julia's `Contract |> Contract`
(dafr: `merge_contracts`) composition operator. 13 PASS, 1 SKIP.

## What changed

### Inline behavior fixes

None. dafr's `merge_contracts` already handles all the expectation-
pair compositions Julia's tests cover (left-fills-right, required-
beats-optional, output-categories preserved, output-output incompatibility
errors with the right wording).

### Documented divergences (1 ID, 1 skip)

| ID | Gap | Notes |
|----|-----|-------|
| C1 | dafr's `.merge_types` uses a total width order; Julia rejects sibling types like Int64 vs Int32. R has no Int32/Int64 distinction at the contract type level. | Conceptual; not a bug per se |

## Files touched

- `tests/testthat/test-contracts-add-jl-parity.R` — new, ~180 lines.
- `dev/notes/2026-05-07-contracts-add-jl-parity-divergences.md` — new.
- `dev/notes/2026-05-07-slice-contracts-add-jl-parity-exit.md` — this file.

No `R/` files touched.

## Remaining contracts.jl

Contracts.jl has 5 more sub-groups (lines 116-1638), each with its
own cross-product structure of (kind × overwrite × direction ×
access-state × type) leaves:

- contracts/scalar (~30 leaves) — verify scalar in / out / missing /
  !type
- contracts/axis (~30 leaves) — verify axis spec
- contracts/vector (~50 leaves) — verify vector + axis-presence
  prerequisites
- contracts/matrix (~80 leaves) — verify matrix + the largest
  cross-product, plus a `fill` sub-block testing the `empty_*` builder
  API which dafr doesn't have (similar to chains slice's C3)
- contracts/tensor (~10 leaves) — tensor wrapper variants

Each is its own future slice. Recommend reading the next sub-group
first to gauge effort before committing — `scalar` is the smallest
and most mechanical; `matrix` is the heaviest and worth its own pair
of slices.

Ready to ship.
