# Slice 9d-N — Exit note

**Date:** 2026-04-22 → 2026-04-23
**Predecessor:** Slice 9d-M (tag `slice-9d-m` on `main` at `676386e`).
**Branch:** `slice-9d-n-csc-axis0-memory-fix` → merged to `main` as
`slice-9d-n`.
**PR:** https://github.com/tanaylab/dafr/pull/2
**Kickoff:** `dev/notes/slice-9d-n-kickoff.md`
**Design:** `dev/notes/2026-04-22-slice-9d-n-design.md`
**Plan:** `dev/plans/2026-04-22-slice-9d-n-csc-axis0-memory-fix.md`

## Scope delivered

6A bundled. All six non-grouped CSC kernels in scope got the
row-partition treatment in a single slice, against the originally
considered alternative of splitting category A and B into two
slices:

**Category A (thread-bucket removed):**
- `kernel_var_csc` — axis=0 branch
- `kernel_minmax_csc` — axis=0 branch
- `kernel_log_reduce` — axis=0 branch
- `kernel_geomean_csc` — axis=0 branch

**Category B (serial fill parallelised + pre-sized):**
- `kernel_mode_csc` — axis=0 branch
- `kernel_quantile_csc` — axis=0 branch

All category-B kernels additionally got a serial nnz-per-row count +
`vector::reserve` pre-sizing pass. The follow-up was added mid-slice
after profiling exposed a `push_back` capacity-doubling contention
issue under 128 threads that ate the parallel-fill benefit; without
pre-sizing the wall win was zero for mode and the memory delta was
noisy (cumulative RSS high-water-mark gave a misleading +6 GB reading
against pre-fix in the profile script, which disappeared in isolated
measurement). The in-slice fix made the per-kernel wall wins clean
and consistent.

## Numbers — 100k × 5k × 0.02 fixture, 128 threads

| kernel | pre wall | post wall | speedup | notes |
|---|---:|---:|---:|---|
| `kernel_var_csc` (Var) | 0.113s | 0.026s | **4.3×** | RSS delta: 868 MB → 672 MB (−196 MB) |
| `kernel_minmax_csc` (Max) | 0.086s | 0.024s | **3.6×** | RSS 0 delta (masked by var's peak) |
| `kernel_log_reduce` (Sum) | 0.086s | 0.028s | **3.1×** | same |
| `kernel_geomean_csc` | 0.087s | 0.024s | **3.6×** | same |
| `kernel_mode_csc` | 0.275s | 0.108s | **2.5×** | RSS 8118 MB → 8251 MB (+133 MB, noise) |
| `kernel_quantile_csc` (q=0.5) | 0.138s | 0.044s | **3.1×** | RSS 0 delta |

Process peak RSS at 128 threads unchanged (694 MB pre and post) —
the var_csc bucket was the single largest allocator in the pre-fix
sweep; removing it opens headroom but the Matrix fixture construction
still dominates the overall process peak.

**Baseline vs post-fix test suite:** `FAIL 1 | WARN 1 | SKIP 0 |
PASS 1931` with `NOT_CRAN=true` (1909 baseline + 22 new stress-test
assertions from 9d-N's bit-identity + RSS budget blocks and the
9d-M stress tests firing under the same env).

## Issue encountered mid-slice: mode_csc RSS artifact

At post-fix profile time the cumulative script reported mode_csc's
RSS delta as ~14 GB, suggesting a 6 GB regression vs pre-fix 8 GB.
Isolated per-kernel measurement (fresh Rscript, gc before the call)
told a cleaner story: pre-fix ~9 GB isolated, post-fix ~8.3 GB
isolated — essentially unchanged.

**Root cause of the artifact:** `bench::bench_process_memory()` max
is monotonic since process start. In the cumulative script each
kernel's "delta" is the amount by which it pushes the process high-
water-mark above whatever prior kernels already set. Pre-fix var_csc
set the peak at 868 MB, so mode delta from there to the true peak
was measured as 8118 MB. Post-fix var_csc set the peak at 672 MB,
leaving mode more headroom to push into and a larger reported delta
(14161 MB). Both runs reached similar TRUE peaks; the delta
arithmetic was apples-to-oranges.

Pre-sizing was added independently for a different reason
(capacity-doubling contention wall-time cost, measured at 2.5× for
mode in isolation). It does not meaningfully move RSS either way.

**Out-of-scope memory concern observed:** mode_csc's 8 GB RSS at
this fixture size is dominated by per-row `std::unordered_map`
allocations in the post-process × 128 threads, not by anything in
this slice's scope. Glibc's `MALLOC_ARENA_MAX=1` drops the delta
from ~9 GB to ~860 MB (10× reduction), confirming the bulk is
per-thread arena caching of freed `unordered_map` nodes. A proper
fix would be the flat-storage refactor deferred by kickoff §2 or
a different map backend; both are follow-up work and remain
deferred from 9d-N as intended.

## Test additions

`tests/testthat/test-kernel-csc-axis0-memory.R` — two `test_that`
blocks:

1. **Bit-identity guard** — across all six row-partitioned kernels
   at axis=0, parallel dispatch (`threshold = 1L`) produces output
   identical to serial dispatch (`threshold = .Machine$integer.max`)
   on a 2k × 2k × 0.02 fixture. Post-fix each row is owned by one
   thread scanning columns in ascending order, so floating-point
   summation order matches the serial path.
2. **Peak-RSS budget** — `kernel_var_csc` axis=0 at a 100k × 5k ×
   0.02 fixture stays under 100 MB delta. Pre-fix pattern (128
   threads × 100k × 16 B = 200 MB bucket) would fail this.

Both use `skip_on_cran()` and run on dev hardware only — matches the
9d-M stress-test convention. CI's `--as-cran` invocation skips them.

## Carry-over — unchanged from kickoff

- **mmap S7-ctor floor** — 4 accept-class breaches from 9c/9d-M
  exits. Architectural, separate slice. Unchanged.
- **Two-pass flat-storage optimisation for mode/quantile per-cell
  overhead** — the `vector<Entry>` / `unordered_map` header cost
  that 9d-M and 9d-N both explicitly deferred. Independent of this
  slice. Now has concrete evidence (8 GB allocator-cache footprint
  at 100k rows) that future work on it would be high-value.
- **Acc-struct slimming** — orthogonal constant-factor work.
- **`copy_all` double-write bug** — small focused fix.
- **9d-M code-review minor items** — deferred follow-up (unused
  `using`, redundant `std::sort`). Still deferred; could fold into
  a housekeeping slice.

## What worked

- **Literal 9d-M template** — the row-partition pattern was
  mechanical enough that six kernel edits went green on first
  compile with one subagent dispatch. The exact-code-in-plan
  pattern from 9d-M carried over cleanly.
- **Isolated-vs-cumulative measurement** — sanity-checking the
  mode_csc "regression" by reframing the RSS arithmetic avoided
  a wrong fix (would have reverted mode parallelization for a
  non-existent regression).
- **Pre-sizing follow-up** — turning a theoretical "parallelism
  enabling" change into a measurable 2.5× wall win for mode,
  without scope creep.

## What didn't

- **Comment accuracy on the pre-sizing commit** — the first draft
  claimed the reserve + count pass was a memory optimisation based
  on a per-thread-arena-fragmentation hypothesis. Isolated
  measurements falsified that; the corrected comment says it's a
  wall-time optimisation (capacity-doubling contention fix). Would
  have avoided churn by measuring first, hypothesising second.
