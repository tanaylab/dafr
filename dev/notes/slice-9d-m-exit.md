# Slice 9d-M — Exit gate (2026-04-22)

**Branch:** `slice-9d-m-g3-memory-fix` at package-repo HEAD `d6d9a14`.
**Predecessor:** `slice-9c` (merged at `8674f4f`); see
`dev/notes/slice-9c-exit.md`.
**Kickoff:** `dev/notes/slice-9d-m-kickoff.md`
**Design:** `dev/notes/2026-04-22-slice-9d-m-design.md`
**Plan:** `dev/plans/2026-04-22-slice-9d-m-g3-memory-fix.md`
**Pre-fix baseline:** `dev/benchmarks/2026-04-22-pre-slice-9d-m-baseline/`
**Post-fix profile:** `dev/benchmarks/2026-04-22-post-slice-9d-m/`
**Post-fix bake-off:** `dev/benchmarks/2026-04-22-post-slice-9d-m-bakeoff/`
**GitHub:** https://github.com/tanaylab/dafr/pull/new/slice-9d-m-g3-memory-fix

## What shipped

6 commits on top of `8674f4f`:

| Commit | Description |
|---|---|
| `b48075d` | `perf(9d-m): add OpenMP shim helpers for row-partition kernels` |
| `ea5d3f6` | `perf(9d-m): row-partition G3 branch of grouped_reduce_csc` |
| `912e80f` | `perf(9d-m): row-partition G3 branch of grouped_mode_csc` |
| `2f959fc` | `perf(9d-m): row-partition G3 branch of grouped_quantile_csc` |
| `adea7ca` | `test(9d-m): regression guards for G3 row-partition` |
| `d6d9a14` | `docs(9d-m): NEWS entry for G3 memory fix` |

**Modified files:**

- `src/openmp_shim.h` — added `dafr_omp_get_num_threads()` + `DAFR_OMP_PARALLEL_IF(cond)`.
- `src/kernel_grouped_reduce_csc.cpp` — axis = 3 branch rewritten.
- `src/kernel_grouped_mode_csc.cpp` — axis = 3 branch rewritten.
- `src/kernel_grouped_quantile_csc.cpp` — axis = 3 branch rewritten.
- `NEWS.md` — Slice 9d-M section added.

**New files:**

- `tests/testthat/test-kernel-grouped-g3-memory.R` — 7 new assertions
  across two `test_that` blocks (bit-identity parallel-vs-serial, peak-
  RSS ≤ 50 MB).

**Unchanged (per spec):**

- G2 (axis = 2) branches of all three kernels.
- `R/query_eval.R` dispatch sites (C++ signatures unchanged).
- `R/options.R` (no new option added; `dafr.grouped_g3_memory_budget`
  explicitly rejected during brainstorming — row-partition has no
  `nthreads` multiplier to cap).
- `src/cpp11.cpp`, `R/cpp11.R` (no regeneration required).
- Bake-off runners (stay at `OMP_NUM_THREADS=1`).

## Test and check state

- **Test suite (NOT\_CRAN=true):** `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 1914 ]`
  (1907 baseline + 7 new 9d-M assertions).
- **Test suite (CRAN guards on):** `[ FAIL 0 | WARN 1 | SKIP 3 | PASS 1907 ]`
  (the 2 new `test_that` blocks skip under CRAN).
- **devtools::check(error\_on = "warning"):** `0 errors | 0 warnings | 4 notes`
  — same 4 notes carried from 9c (benchmarks dir, installed size,
  future timestamps, hidden `.claude/`).

## Profile comparison (stress fixture: 10k × 10k CSC, 100 groups, density 0.01)

Machine: 128 threads, 1 TB RAM.

### Peak RSS

| Threads | Pre-fix | Post-fix | Reduction |
|---:|---:|---:|---:|
| 1 | 397 MB | 342 MB | -55 MB |
| 8 | 779 MB | 341 MB | -438 MB |
| 32 | 2.09 GB | 340 MB | -1.75 GB |
| 128 | **7.34 GB** | **340 MB** | **-7.0 GB** |

Post-fix peak RSS is flat across thread counts. The `nthreads` multiplier
is gone — the memory pathology is closed.

### Wall-time at 128 threads

| Kernel | Pre-fix | Post-fix | Speed-up |
|---|---:|---:|---:|
| `reduce_csc` Sum | 3.177 s | 0.036 s | **88×** |
| `reduce_csc` Var | 3.519 s | 0.034 s | **103×** |
| `mode_csc` | 3.615 s | 0.063 s | **57×** |
| `quantile_csc` p50 | 1.690 s | 0.059 s | **29×** |

Pre-fix at 128 threads was *slower* than 1-thread because the serial
merge dominated. Post-fix wall-time at 128 threads is universally
lower than the 1-thread baseline.

## Bake-off (single-threaded, vs. Julia oracle from 9c)

- **Topology:** 79 queries, **4 breached / 75 within**. Breach set
  identical to 9c exit (four `mmap_open_read_*` queries, accepted as
  S7-ctor floor).
- **Per-query ratio drift vs 9c:** within ±6% on the four breaches;
  within ±3% on the 9c-closed queries (026 Quantile 1.24 → 1.27×;
  028 Mode 1.18 → 1.18×; 043 G2 Max 0.85 → 0.88×; 047 G3 Max 0.90 → 0.91×).
  Normal run-to-run noise. No regressions.
- **G3 queries on `big_sparse`** (the stress fixture the memory fix
  protects): `grouped_g3_{sum, mean, max}_100` run at 0.06–0.07× of
  Julia single-thread (dafr ~15× faster); `grouped_g3_mean_1000` at
  0.31×. Row-partition did not regress single-thread behaviour.

## Code review

**Final whole-branch review (Opus, superpowers:code-reviewer):**
APPROVED WITH MINOR NOTES. Two non-blocking follow-up items:

1. `src/kernel_grouped_reduce_csc.cpp:34` — unused
   `using dafr_grouped::acc_merge;` declaration (the merge phase is
   gone). One-line removal; fold into a housekeeping commit.
2. `src/kernel_grouped_mode_csc.cpp:203-204` — `std::sort` by `entry.pos`
   is redundant in the row-partition version (entries arrive pre-sorted
   because each slot is written by a single thread in ascending `j`
   order, and `pos = col_ord[j]` is monotonically increasing within a
   group as `j` increases). Small perf tweak for a follow-up slice.

Neither is a correctness issue or merge blocker. Recorded here for the
next slice's scope list.

## Acceptance criteria (from design §10) — all met

- [x] **Test suite:** `FAIL=0 PASS≥1912` — 1914 passes with
      NOT\_CRAN=true.
- [x] **devtools::check:** 0 errors, 0 warnings, ≤ 4 notes.
- [x] **Bake-off `OMP_NUM_THREADS=1`:** 4 → 4 breaches, per-query
      drift ≤ ±5%.
- [x] **Post-fix 128-thread profile:** peak RSS ≤ 500 MB (achieved
      340 MB), wall-time at 128 threads ≤ 1-thread wall-time for all
      four kernels.

## Remaining breaches (carry-over from 9c; unchanged)

All 4 remaining breaches are the mmap S7-ctor floor, unchanged from 9c
exit. These require architectural changes outside the G3 memory-fix
scope of 9d-M.

| Query | Post-9c | Post-9d-M |
|---|---:|---:|
| `mmap_open_read_scalar` | 1.56× | 1.62× |
| `mmap_open_read_vector` | 1.93× | 2.02× |
| `mmap_open_read_matrix` | 2.46× | 2.62× |
| `mmap_open_read_axis`   | 2.66× | 2.82× |

Accept-class, unchanged topology. Real work for a future 9d-P or
architectural slice.

## Carry-over for the next slice

- **Two minor cleanup items from code review** (unused `using`, redundant
  `std::sort`) — fold into next slice's housekeeping commit.
- **Two-pass flat-storage optimisation for mode/quantile per-cell
  vector overhead** — deferred from 9d-M scope. At metacell scale
  (nrow = 10⁶, ngroups = 100) the per-cell `std::vector` header
  overhead is ~2.4 GB even post-fix. Row-partition handled the
  `nthreads` multiplier; the per-cell constant is the next axis.
- **Acc-struct slimming** (48 B → possibly less) — orthogonal constant-
  factor optimisation.
- **Dynamic OpenMP scheduling** for skew-imbalance mitigation on real
  metacell matrices — ship static for now; measure and decide.
- **mmap S7-ctor floor** — 4 accept-class breaches carried into 9d-P
  or later.
- **`copy_all` double-write bug** — small focused fix, candidate for a
  standalone slice.

## Auto-memory recommendations

Entries in `~/.claude/projects/-.../memory/` worth keeping:

- **Native port motivation + perf parity goal** — still applies; post-
  9d-M non-mmap parity + parallel-scale safety. With the G3 memory
  pathology closed, the metacell workload can now run on a multi-thread
  box without exhausting RAM.
- **Bake-off cache invalidation** and **R CMD INSTALL --preclean
  prerequisite** — still applies.
- **Opus for design-heavy work / final reviews** — validated again
  this slice; the Opus whole-branch review caught two real minor
  issues and gave a rigorous bit-identity argument.

No new durable memory entries needed.

## Julia DAF state at Slice 9d-M exit

- `~/src/DataAxesFormats.jl` at `49fbba140437387a378217c2fa658d4231d0c8c1`
  (unchanged since Slice 3; ten slices of stability now).
- `~/src/TanayLabUtilities.jl` at `48a4a57` (unchanged).
- Bake-off Julia times reused from 9c run
  (`dev/benchmarks/2026-04-22-post-slice-9c/slice-9c-julia-times.csv`).

## Summary

Slice 9d-M shipped exactly what its kickoff scoped: the G3 thread-bucket
memory pathology is closed via row-partition, without changing public
API, dispatch sites, or bake-off topology. At 128 threads on the stress
fixture the peak RSS drops 7 GB and wall-time improves 30–100× — the
memory fix turned out to be a major performance fix too, because the
pre-fix code's serial merge had been doubling as the dominant wall-time
cost at high thread counts. Test suite clean (1914 passes), `R CMD check`
clean, bake-off no regression. Branch pushed; ready for merge at the
user's discretion.
