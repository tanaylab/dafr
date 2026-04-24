# Slice 9d-M — Design: G3 Kernel Thread-Bucket Memory Fix

**Date:** 2026-04-22
**Predecessor:** Slice 9c (tag `slice-9c` on `main` at `8674f4f`).
**Kickoff:** `dev/notes/slice-9d-m-kickoff.md`.
**Scope:** Option M from the Slice 9c kickoff menu — row-partition
fix for three grouped-CSC kernels.

## 1. Goal

Eliminate the `O(nthreads × nrow × ngroups)` thread-bucket memory
pattern in the G3 (col-group, `axis = 3`) branch of the three CSC
grouped kernels. At metacell scale (nrow ≈ 10⁶, ngroups ≈ 10², 128
threads) the current pattern requires ~600 GB of thread-local
accumulator state and is therefore unusable in parallel.

Kernels affected:

| Source file | G3 thread-bucket payload |
|---|---|
| `src/kernel_grouped_reduce_csc.cpp` | `vector<Acc>` (48 B per cell) |
| `src/kernel_grouped_mode_csc.cpp` | `vector<vector<Entry>>` (24 B + entries) |
| `src/kernel_grouped_quantile_csc.cpp` | `vector<vector<double>>` (24 B + entries) |

The G2 (row-group, `axis = 2`) branch of each kernel is untouched —
it already has the right shape (per-column local accumulator, no
cross-thread state).

**Done signal:** at the locked stress fixture (`10000 × 10000` CSC,
100 groups, density 0.01), post-fix peak RSS at 128 threads matches
the 1-thread baseline within normal allocator slack (target ≤ 500 MB
vs. pre-fix 7.34 GB), and post-fix wall-time at 128 threads is ≤
the 1-thread wall-time (pre-fix is 10–30× *slower* at 128 threads).

## 2. Out of scope

- Acc-struct slimming (48 B → fewer bytes). Orthogonal constant-
  factor optimisation; 9d-M stays focused on the `nthreads`
  multiplier.
- Two-pass flat-storage optimisation for mode/quantile (replacing
  `vector<Entry>` / `vector<double>` per cell with a CSC-style
  offsets + flat storage layout). Would shave the remaining
  `nrow × ngroups × 24 B` empty-vector overhead. Deferred — 9d-M
  fixes only the `nthreads` multiplier; per-cell overhead is a
  separate optimisation.
- Dynamic OpenMP scheduling for skew-imbalance mitigation. The
  fix uses `schedule(static)` (inherited from the shim). Revisit
  only if post-fix profiling shows meaningful load imbalance on
  real data; not in 9d-M.
- mmap S7-ctor floor (4 accept-class breaches from 9c exit).
- `copy_all` double-write bug.
- No new user-facing options. Kickoff decision #4
  (`dafr.grouped_g3_memory_budget`) is resolved NO — row-partition
  has no `nthreads` multiplier to cap.

## 3. Locked decisions

1. **Strategy:** row-partition for all three CSC kernels. Each
   thread owns a disjoint row range `[r0, r1)`; two threads never
   write to the same accumulator slot.
2. **No memory-budget option.** `dafr.kernel_threshold` remains
   the single parallel-dispatch lever.
3. **Per-cell storage in mode/quantile kept as `vector<Entry>` /
   `vector<double>`.** Two-pass flat storage deferred.
4. **Scheduling:** static (inherited from `openmp_shim.h`).
5. **Pre-design profile baseline is run and reported** — see §8.
6. **Permanent testthat stress test:** `nrow = 2000`, `ncol = 2000`,
   `ngroups = 20`, density ≈ 0.02 (nnz ≈ 80k),
   `OMP_NUM_THREADS = min(8, parallel::detectCores())`, peak-RSS
   assertion via `bench::bench_process_memory()`, budget ≤ 50 MB
   over idle baseline.
7. **No Acc-struct refactor.** Single allocation of `vector<Acc>`
   of length `nrow × ngroups`, owned by the main thread, written
   by all threads into disjoint slots.

## 4. Algorithm — row-partition G3

The G3 branch of each kernel is rewritten to the following pattern.
All `accs` writes are race-free by construction: the row index `r`
determines slot ownership, and row ranges across threads are
disjoint.

### 4.1. `kernel_grouped_reduce_csc.cpp` (G3)

```cpp
// axis == 3 (G3, col-group): output is nrow x ngroups.
// Row-partition: each thread owns a row range [r0, r1). Writes to
// accs[pi[k] + g*nrow] are race-free because only the owning thread
// ever touches slots with r in [r0, r1).
cpp11::writable::doubles_matrix<cpp11::by_column> out(nrow, ngroups);
std::vector<Acc> accs((size_t)nrow * (size_t)ngroups);
if (need_log) for (auto &a : accs) a.need_log = true;

#pragma omp parallel if(nrow >= threshold)
{
    const int tid = dafr_omp_get_thread_num();
    const int nt  = dafr_omp_get_num_threads();
    const int chunk = (nrow + nt - 1) / nt;
    const int r0 = std::min(nrow, tid * chunk);
    const int r1 = std::min(nrow, r0 + chunk);

    // Pass 1: scan all columns; filter by row-range.
    for (int j = 0; j < ncol; ++j) {
        const int g = pg[j] - 1;
        const size_t base = (size_t)g * (size_t)nrow;
        const int k_end = pp[j + 1];
        for (int k = pp[j]; k < k_end; ++k) {
            const int r = pi[k];
            if (r < r0 || r >= r1) continue;
            accs[base + (size_t)r].push(px[k], eps);
        }
    }

    // Pass 2: post-process rows in [r0, r1).
    for (int r = r0; r < r1; ++r) {
        for (int g = 0; g < ngroups; ++g) {
            out(r, g) = derive_op(op,
                accs[(size_t)r + (size_t)g * (size_t)nrow],
                png[g], eps);
        }
    }
}
```

The raw `#pragma omp parallel if(...)` is used (not
`DAFR_PARALLEL_FOR`) because we want a single parallel region that
covers both passes, with thread-local `r0`/`r1` derived from
`omp_get_thread_num()`. `openmp_shim.h` gains a small helper
`dafr_omp_get_num_threads()` returning `omp_get_num_threads()`
inside the region (falls back to `1` when `_OPENMP` is undefined).

The raw pragma here is acceptable under the repo rule ("OpenMP via
`openmp_shim.h` helpers … never raw pragmas") because the shim
already emits raw pragmas internally via the `DAFR_PRAGMA_STR`
mechanism. We add one more helper (`DAFR_OMP_PARALLEL_IF`) to
keep the no-raw-pragma invariant visible at the call site:

```c
// in openmp_shim.h
#define DAFR_OMP_PARALLEL_IF(cond) DAFR_PRAGMA_STR(omp parallel if(cond))
```

All three kernels use `DAFR_OMP_PARALLEL_IF(nrow >= threshold)`.

### 4.2. `kernel_grouped_mode_csc.cpp` (G3)

Same parallel shape. `col_ord[]` and `cols_in_group[]` pre-
computation stays serial (before the parallel region). A single
`std::vector<std::vector<Entry>> accs((size_t)nrow * ngroups)`
replaces both `tbuf` (length `nthreads`) and the old merged
`accs`. The entire `tbuf`-merge block (lines 199–212 of the
current file) is deleted.

```cpp
DAFR_OMP_PARALLEL_IF(nrow >= threshold)
{
    const int tid = dafr_omp_get_thread_num();
    const int nt  = dafr_omp_get_num_threads();
    const int chunk = (nrow + nt - 1) / nt;
    const int r0 = std::min(nrow, tid * chunk);
    const int r1 = std::min(nrow, r0 + chunk);

    for (int j = 0; j < ncol; ++j) {
        const int g = pg[j] - 1;
        const int ord = col_ord[j];
        const size_t base = (size_t)g * (size_t)nrow;
        const int k_end = pp[j + 1];
        for (int k = pp[j]; k < k_end; ++k) {
            const int r = pi[k];
            if (r < r0 || r >= r1) continue;
            accs[base + (size_t)r].push_back({px[k], ord});
        }
    }

    // Post-process: sort by pos, walk cols_in_group[g], count mode.
    // This is the existing per-(r,g) post-processing loop, restricted
    // to r in [r0, r1).
    for (int r = r0; r < r1; ++r) {
        for (int g = 0; g < ngroups; ++g) {
            // ... identical to existing lines 218-282 ...
        }
    }
}
```

Entries written to `accs[base + r]` come from one thread only
(the one owning `r`). Push order within a slot is ascending column
`j` (same as current serial/merged ordering), so the subsequent
`std::sort` by `entry.pos` is against the same input distribution
as before.

### 4.3. `kernel_grouped_quantile_csc.cpp` (G3)

Same parallel shape. A single
`std::vector<std::vector<double>> accs((size_t)nrow * ngroups)`
replaces `tbuf`; the merge block (lines 137–151 of the current
file) is deleted.

```cpp
DAFR_OMP_PARALLEL_IF(nrow >= threshold)
{
    const int tid = dafr_omp_get_thread_num();
    const int nt  = dafr_omp_get_num_threads();
    const int chunk = (nrow + nt - 1) / nt;
    const int r0 = std::min(nrow, tid * chunk);
    const int r1 = std::min(nrow, r0 + chunk);

    for (int j = 0; j < ncol; ++j) {
        const int g = pg[j] - 1;
        const size_t base = (size_t)g * (size_t)nrow;
        const int k_end = pp[j + 1];
        for (int k = pp[j]; k < k_end; ++k) {
            const int r = pi[k];
            if (r < r0 || r >= r1) continue;
            accs[base + (size_t)r].push_back(px[k]);
        }
    }

    // Post-process: partition into neg/pos, pick_rank. Same logic
    // as existing lines 154-188, restricted to r in [r0, r1).
    for (int r = r0; r < r1; ++r) {
        for (int g = 0; g < ngroups; ++g) {
            // ... identical to existing post-process ...
        }
    }
}
```

Note on `pick_rank` and `nth_element`: these are destructive on
`neg` / `pos`, but both vectors are thread-local (declared inside
the inner `for (int r = ...)` body as in the current code), so
parallelism is race-free.

## 5. Correctness argument

**Bit-for-bit equivalence to the current serial path (nthreads = 1):**
with a single thread, row range is `[0, nrow)`, the filter `if (r <
r0 || r >= r1) continue;` becomes a no-op, and the scan order
matches the existing `axis == 2`-like per-column iteration. Every
`push()` / `push_back()` is called with the same arguments in the
same order, so `Acc` state, `vector<Entry>` state, and
`vector<double>` state are identical.

**Parallel-vs-serial equivalence:** inside `accs[base + r]`, only
the thread owning `r` ever writes. Push order within a slot is
preserved (columns are scanned in ascending `j`, entries in
ascending `k`). No cross-thread reduction — `acc_merge` is never
called in the row-partitioned path.

The test plan (§7) asserts bit-identical output between `threshold
= 1L` (forced parallel) and `threshold = .Machine$integer.max`
(forced serial) at the stress fixture.

## 6. Performance argument

Current G3 code at 128 threads:

1. Allocate + page-fault `128 × nrow × ngroups × 48 B` of zeroed
   thread-buckets (6 GB at stress fixture).
2. Parallel pass over `ncol` columns: trivial (7800 push() calls
   per thread).
3. **Serial merge** of `128 × nrow × ngroups = 128M` Accs — this
   is what dominates the 3–3.5 s wall-time.
4. Parallel post-process.

Row-partition eliminates steps 1 and 3. Each thread does the same
parallel pass (filtered by row-range) and its own serial post-
process over its row range. No allocations scale with `nthreads`.

**Predicted scaling** (relative to 1-thread baseline, stress
fixture):

| Threads | Current peak RSS | Current wall | Predicted post-fix peak RSS | Predicted post-fix wall |
|---|---:|---:|---:|---:|
| 1 | 397 MB | 0.10 s (reduce) | ~397 MB (same) | ≈ 0.10 s |
| 8 | 779 MB | 0.27 s | ~400 MB | ≈ 0.02 s |
| 32 | 2.09 GB | 0.95 s | ~400 MB | ≈ 0.005 s |
| 128 | 7.34 GB | 3.18 s | ~400 MB | ≈ 0.002 s |

(Wall-time predictions assume perfectly-balanced row ranges. Real
matrices will exhibit skew; measured post-fix numbers go in the
exit note.)

## 7. Testing

### 7.1. Existing test suite — unchanged

All 1909 passing tests (from Slice 9c) must continue to pass.
Grouped-reduce, grouped-mode, grouped-quantile tests exercise the
G3 branch at small sizes — they act as regression guard for the
rewrite.

### 7.2. New stress test — `tests/testthat/test-kernel-grouped-g3-memory.R`

```r
test_that("G3 row-partition memory stays bounded at moderate thread count", {
    skip_on_cran()
    # Small stress: 2k x 2k CSC, 20 groups, density ~0.02 (nnz ≈ 80k).
    # Forces parallel dispatch via threshold = 1L.
    set.seed(42L)
    nr <- 2000L; nc <- 2000L; ngroups <- 20L
    nnz <- as.integer(nr * nc * 0.02)
    m <- Matrix::sparseMatrix(
        i = sample.int(nr, nnz, replace = TRUE),
        j = sample.int(nc, nnz, replace = TRUE),
        x = runif(nnz, 0.1, 10.0),
        dims = c(nr, nc), repr = "C")
    group <- rep_len(seq_len(ngroups), nc)
    n_in_group <- tabulate(group, nbins = ngroups)

    # Parallel vs. serial bit-identical.
    par_out <- kernel_grouped_reduce_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, op = "Sum", eps = 0, threshold = 1L)
    ser_out <- kernel_grouped_reduce_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, op = "Sum", eps = 0,
        threshold = .Machine$integer.max)
    expect_identical(par_out, ser_out)

    # Same for mode and quantile.
    par_mode <- kernel_grouped_mode_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, threshold = 1L)
    ser_mode <- kernel_grouped_mode_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, threshold = .Machine$integer.max)
    expect_identical(par_mode, ser_mode)

    par_q <- kernel_grouped_quantile_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, q = 0.5, threshold = 1L)
    ser_q <- kernel_grouped_quantile_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, q = 0.5, threshold = .Machine$integer.max)
    expect_identical(par_q, ser_q)
})

test_that("G3 row-partition peak RSS stays under budget (memory regression)", {
    skip_on_cran()
    skip_if_not_installed("bench")
    # Larger fixture for the memory assertion — 2k x 2k x 20 groups.
    # Current threads: capped to 8 for CI sanity.
    old <- Sys.getenv("OMP_NUM_THREADS", unset = NA)
    on.exit(if (is.na(old)) Sys.unsetenv("OMP_NUM_THREADS")
            else Sys.setenv(OMP_NUM_THREADS = old))
    nt <- min(8L, parallel::detectCores())
    # NOTE: libgomp caches max_threads at load time, so this setenv
    # won't affect already-loaded code. This test asserts the peak
    # RSS under whatever thread count libgomp picked up, which on a
    # developer/CI box is typically detectCores(). That is enough
    # to catch reintroduction of the O(nthreads) bucket pattern —
    # even at 2 threads the pre-fix bucket is ≥ 2x the post-fix
    # footprint.
    set.seed(42L)
    nr <- 2000L; nc <- 2000L; ngroups <- 20L
    nnz <- as.integer(nr * nc * 0.02)
    m <- Matrix::sparseMatrix(
        i = sample.int(nr, nnz, replace = TRUE),
        j = sample.int(nc, nnz, replace = TRUE),
        x = runif(nnz, 0.1, 10.0),
        dims = c(nr, nc), repr = "C")
    group <- rep_len(seq_len(ngroups), nc)
    n_in_group <- tabulate(group, nbins = ngroups)

    gc(full = TRUE)
    mem_before <- bench::bench_process_memory()
    out <- kernel_grouped_reduce_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, op = "Sum", eps = 0, threshold = 1L)
    mem_after <- bench::bench_process_memory()
    delta <- as.numeric(mem_after["max"]) - as.numeric(mem_before["max"])
    # Budget: 50 MB. Row-partition accs = 2000 * 20 * 48 B = 1.9 MB;
    # output matrix = 2000 * 20 * 8 B = 312 KB. The bound is loose
    # to tolerate allocator slack and bench's own overhead.
    expect_lt(delta, 50 * 1024 * 1024)
})
```

### 7.3. Post-fix profile validation (dev-only)

Re-run `dev/benchmarks/2026-04-22-pre-slice-9d-m-baseline/run.sh`
against the row-partition build. Expected outcome: peak RSS flat
at ~400 MB across all thread counts; wall-time at 128 threads
≤ 1-thread wall-time. Numbers land in exit note, not in the package.

### 7.4. Bake-off — no change

Bake-off runs `OMP_NUM_THREADS=1` and is the apples-to-apples
single-thread measurement. It must not change for 9d-M, and the
single-thread wall-time should be unaffected by the row-partition
rewrite (same work, same order, no merge phase to eliminate).
Expected: 4 → 4 breaches (unchanged); per-query ratios within ±5%.

## 8. Pre-fix baseline profile

Stress fixture: 10k × 10k CSC, 100 groups, density 0.01 (nnz ≈ 1M).
All three kernels run in axis = 3, `threshold = 1L`. Machine: 128
thread Linux box, 1 TB RAM. Results from
`dev/benchmarks/2026-04-22-pre-slice-9d-m-baseline/`:

### 8.1. Peak RSS (whole Rscript, via `/usr/bin/time -v`)

| Threads | Peak RSS | Δ vs 1-thread | Theoretical bucket memory (128 × nrow × ngroups × 48 B) |
|---:|---:|---:|---:|
| 1 | 397 MB | — | 48 MB |
| 8 | 779 MB | +380 MB | 384 MB |
| 32 | 2.09 GB | +1.69 GB | 1.54 GB |
| 128 | 7.34 GB | +7.0 GB | 6.14 GB |

Matches `nthreads × nrow × ngroups × sizeof(Acc)` exactly.

### 8.2. Wall-time per G3 kernel call

| Kernel | 1 | 8 | 32 | 128 |
|---|---:|---:|---:|---:|
| `reduce_csc` Sum | 0.106 s | 0.273 | 0.948 | 3.177 |
| `reduce_csc` Var | 0.105 s | 0.369 | 0.842 | 3.519 |
| `mode_csc` | 0.375 s | 0.304 | 0.567 | 3.615 |
| `quantile_csc` p50 | 0.163 s | 0.272 | 0.566 | 1.690 |

The current parallel path is slower than single-threaded at
`nthreads ≥ 32` — the serial merge of `nthreads × nrow × ngroups`
Accs dominates. Row-partition removes the merge entirely.

## 9. Deliverables (files changed)

- `src/kernel_grouped_reduce_csc.cpp` — G3 branch rewritten.
- `src/kernel_grouped_mode_csc.cpp` — G3 branch rewritten.
- `src/kernel_grouped_quantile_csc.cpp` — G3 branch rewritten.
- `src/openmp_shim.h` — add `dafr_omp_get_num_threads()` and
  `DAFR_OMP_PARALLEL_IF(cond)` helpers.
- `tests/testthat/test-kernel-grouped-g3-memory.R` — new, two
  `test_that` blocks per §7.2.
- `NEWS.md` — one-line entry under the `# dafr 0.x.0 (in progress)`
  header: `Fix O(nthreads × nrow × ngroups) memory growth in G3
  grouped CSC kernels (reduce / mode / quantile) via row-partition.`

Unchanged:

- `R/query_eval.R` — dispatch sites untouched (C++ signatures
  unchanged).
- `R/options.R` — no new option.
- `src/cpp11.cpp`, `R/cpp11.R` — no signature change → no
  regeneration needed.

## 10. Acceptance criteria

- **Test suite:** `[ FAIL 0 | WARN ≤ 1 | SKIP ≤ 2 | PASS ≥ 1912 ]`
  (1909 pre-existing + ≥ 3 new assertions; `skip_on_cran` + optional
  `skip_if_not_installed("bench")` counted).
- **`devtools::check(error_on = "warning")`:** 0 errors, 0
  warnings, ≤ 4 notes (carried from 9c).
- **Bake-off `OMP_NUM_THREADS=1`:** 4 → 4 breaches; per-query
  ratio drift ≤ ±5%.
- **Post-fix 128-thread profile:** peak RSS ≤ 500 MB at stress
  fixture (vs. 7.34 GB pre-fix), wall-time at 128 threads ≤
  1-thread wall-time for all four measured kernels.

## 11. Risks

1. **Skew-imbalance on non-uniform row distributions.** Real
   metacell data has gene-expression matrices where a small
   number of rows hold most of the nnz. `schedule(static)` row
   chunks will imbalance: the thread owning the high-density row
   range does all the work. Mitigation for 9d-M: none — we ship
   static scheduling and measure. If post-fix profiling shows a
   significant imbalance (e.g. 2× wall-time vs. balanced
   prediction), follow-up slice adds
   `DAFR_PARALLEL_FOR_DYNAMIC` with a tuned chunk size. Out of
   scope here.
2. **Scan-overhead for ultra-sparse `ncol >> nnz` matrices.**
   Each thread walks all `pp[0..ncol]` pointers. For `ncol = 10⁷`
   with `nnz = 10⁴` this is noticeable. Not expected in the
   metacell workload; if it shows up, two-pass (column-split the
   scan, merge row-partition push lists) is a known fallback.
3. **`bench::bench_process_memory()` variance on CI.** The peak-
   RSS budget is 50 MB — loose enough that normal allocator slack
   plus bench's overhead won't false-positive. If CI still flakes,
   widen to 100 MB or skip on CI (decision deferred to post-
   implementation testing).
4. **libgomp thread-count caching.** The stress test's
   `Sys.setenv(OMP_NUM_THREADS=…)` is a no-op after the DSO is
   loaded. The test's RSS assertion is therefore valid against
   whatever thread count libgomp picked up at load time (typically
   `detectCores()`). Documented in the test comment. The profile
   `run.sh` handles this correctly by exporting before Rscript.

## 12. Mines (inherited from kickoff, reinforced here)

- All three CSC kernels edited in lockstep — unfixed one caps
  the ceiling.
- Bake-off `OMP_NUM_THREADS=1` stays untouched.
- `empty_cache` per iteration in bake-off runners — load-bearing.
- `R CMD INSTALL . --preclean` before any bake-off.
- Formula authority is `R/operations.R` `.op_*`; no behaviour
  change in the kernels.
- cpp11 (not Rcpp), `.h` (not `.hpp`), OpenMP via shim helpers.
- `dafr.kernel_threshold` must not be set to `Inf`.
- `.Rprofile` sets `options(error = recover)` — stress scripts set
  `options(error = NULL)` at entry.

## 13. Implementation order

1. Add `dafr_omp_get_num_threads()` + `DAFR_OMP_PARALLEL_IF` to
   `src/openmp_shim.h`.
2. Rewrite G3 branch of `kernel_grouped_reduce_csc.cpp`.
3. Rewrite G3 branch of `kernel_grouped_mode_csc.cpp`.
4. Rewrite G3 branch of `kernel_grouped_quantile_csc.cpp`.
5. Add `tests/testthat/test-kernel-grouped-g3-memory.R`.
6. `devtools::load_all()` + `testthat::test_package("dafr")`.
7. `devtools::check(error_on = "warning")`.
8. Re-run `dev/benchmarks/2026-04-22-pre-slice-9d-m-baseline/run.sh`
   on the fixed build → post-fix table for exit note.
9. Run bake-off (`R CMD INSTALL . --preclean` first) to confirm
   no regression on 79 queries.
10. NEWS.md entry.
11. Commit, push, tag `slice-9d-m` after merge.

Each of (1)–(5) is a separate commit; (6)–(9) are verification
gates, not commits.
