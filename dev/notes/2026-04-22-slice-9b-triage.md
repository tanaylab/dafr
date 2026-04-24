# Slice 9b — Baseline triage (2026-04-22)

**Baseline:** `dev/benchmarks/2026-04-22-baseline/` (dev repo commit
`295a0dd`), against package repo commit `2cc1348`.

Ratio = `dafr_median / julia_median`. Higher = dafr slower. Breach =
`ratio > threshold` for the query's tier.

## Summary

- **79 queries** measured; **17 breached** their tier threshold.
- **62 within threshold**, including all 20 queries on the big_sparse
  10k × 10k corpus — dafr is within or beats Julia on every heavy
  kernel at scale.
- Breaches are concentrated on the tiny `cells_daf` fixture (UMIs is
  683 × 23) where per-query dispatch and kernel-setup overhead is
  proportionally large.

## Dispositions (user-confirmed 2026-04-22)

### Group A — Grouped reductions on UMIs (7 queries) — **Fix**

Queries 41–47 from the julia-queries fixture; G2 and G3 patterns.

| # | Query | ratio |
|---|---|---|
| 41 | `@ cell @ gene :: UMIs -/ experiment >- Sum` | 3.69× |
| 42 | `@ cell @ gene :: UMIs -/ experiment >- Mean` | 4.23× |
| 43 | `@ cell @ gene :: UMIs -/ experiment >- Max` | 4.23× |
| 44 | `@ cell @ gene :: UMIs -/ experiment >- Var` | 3.69× |
| 45 | `@ gene @ cell :: UMIs |/ experiment >| Sum` | 3.96× |
| 46 | `@ gene @ cell :: UMIs |/ experiment >| Mean` | 3.72× |
| 47 | `@ gene @ cell :: UMIs |/ experiment >| Max` | 4.35× |

**Hypothesis:** grouped-reduce CSC kernel does OpenMP setup and a
thread-local bucket allocation even when the input is small (23
columns × N groups). The setup cost dominates at UMIs scale.

**Planned task:** T11.1 — add a small-matrix fast path that skips
OpenMP thread fan-out below a configurable input-size threshold.

### Group B — Var/Std/VarN/StdN/Quantile/Mode on UMIs (6 queries) — **Fix**

Queries 21, 22, 23, 24, 26, 28 from the julia-queries fixture;
column-reduce to a vector on the 683-length cell axis.

| # | Query | ratio |
|---|---|---|
| 21 | `@ cell @ gene :: UMIs >| Var`  | 2.50× |
| 22 | `@ cell @ gene :: UMIs >| Std`  | 2.48× |
| 23 | `@ cell @ gene :: UMIs >| VarN eps 1` | 2.79× |
| 24 | `@ cell @ gene :: UMIs >| StdN eps 1` | 2.25× |
| 26 | `@ cell @ gene :: UMIs >| Quantile p 0.5` | 3.04× |
| 28 | `@ cell @ gene :: UMIs >| Mode` | 2.33× |

**Hypothesis:** same as Group A — small-matrix setup cost for the
per-column sparse reducer. These all go through the same
`kernel_reduce_csc_*` machinery.

**Planned task:** T11.2 — same small-matrix fast path; likely closes
Groups A + B together.

### Group C — `mmap_open_*` queries (4) — **Investigate**

| # | Query | ratio |
|---|---|---|
| — | `open_daf + read matrix` | 2.50× |
| — | `open_daf + read vector` | 1.99× |
| — | `open_daf + read scalar` | 1.73× |
| — | `open_daf + read axis` | 2.64× |

All four are `reopen: true` — each iteration reopens the FilesDaf
directory from scratch. Julia is 0.5–1.5 ms faster per query.

**Hypothesis options (need profile to pick):**
- R's `jsonlite::read_json(daf.json)` is slower than Julia's JSON parse.
- Our `files_daf(..., mode="r")` constructor does more work than
  DAF.jl's `FilesDaf(path; mode="r")` (e.g., full axis-set scan, or
  redundant S7 validators).
- Something in our axis/scalar initial read path.

**Planned task:** T11.3 — profile one open+read query (use
`profvis::profvis()` on a single iteration); then decide fix vs accept.

## Open questions (deferred to exit note unless they become blockers)

- **Why dafr beats Julia on big_sparse kernels** — interesting
  observation but not a bug to fix. Worth noting in the exit summary.
- **G3 kernel memory explosion at 128 threads** — parallel-mode
  profiling is still required per the plan (Task 12.0). Single-thread
  baseline doesn't exercise the memory issue.

## Task shape after triage

- **T11.1** — small-matrix fast path for `kernel_grouped_reduce_csc_*`
  (closes Group A, 7 queries).
- **T11.2** — small-matrix fast path for `kernel_reduce_csc_*` (closes
  Group B, 6 queries). May share implementation with T11.1.
- **T11.3** — `files_daf` open-path profiling + fix (closes Group C,
  4 queries).
- **T12** — G3 kernel memory fix, 128-thread profile on lab machine.
- **T13** — exit.
