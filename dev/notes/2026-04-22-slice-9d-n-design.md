# Slice 9d-N — Design: CSC Axis-0 Thread-Bucket Memory Fix

**Date:** 2026-04-22
**Predecessor:** Slice 9d-M (tag `slice-9d-m` on `main` at `676386e`).
**Kickoff:** `dev/notes/slice-9d-n-kickoff.md`.
**Scope:** Extend the row-partition technique from 9d-M to six
non-grouped CSC kernels — four category-A kernels that carry
`O(nthreads × nrow)` thread-bucket memory patterns, and two
category-B kernels whose axis-0 fill pass is currently single-
threaded to avoid write races.

## 1. Goal

Eliminate two anti-patterns from six CSC kernels:

**Category A (thread-bucket)**: four kernels allocate
`nthreads × nrow`-sized thread-local accumulators, scan columns in
parallel, then serially merge into a single output-sized vector
before the parallel post-process. At metacell scale
(nrow ≈ 10⁶, 128 threads) each kernel carries ~1.5–2 GB of this
overhead; combined peak-RSS ceiling is ~6 GB independent of the
output size.

| Source file | Line range | Bucket payload | Memory at 128 threads × 1 M nrow |
|---|---|---|---:|
| `src/kernel_var_csc.cpp` | `59-61` | `tsx` + `tsxx` (2 × double) | ~2.0 GB |
| `src/kernel_minmax_csc.cpp` | `57-61` | `tbuf` (double) + `tnnz` (int) | ~1.5 GB |
| `src/kernel_log_reduce.cpp` | `90-95` | `tsum` (double) + `tnnz` (int) | ~1.5 GB |
| `src/kernel_geomean_csc.cpp` | `80-86` | `tsum` (double) + `tnnz` (int) | ~1.5 GB |

**Category B (serial fill)**: two kernels' axis-0 fill pass is
forced single-threaded because `rows[pi[k]].push_back(...)` would
race across threads. Post-process is already parallel. Row-partition
makes the fill pass parallel with no memory penalty.

| Source file | Line range | Why currently serial |
|---|---|---|
| `src/kernel_mode_csc.cpp` | `118-123` | Writes to `rows[pi[k]]` would race |
| `src/kernel_quantile_csc.cpp` | `118-125` | Explicit comment: *"No parallelism here; writes to `rows[pi[k]]` would race across threads."* |

**Done signal:** at the locked stress fixture (`nrow = 100000`,
`ncol = 5000`, density 0.02, ~10M nnz), post-fix peak-RSS delta
across one representative category-A kernel at 128 threads is
≤ 100 MB; pre-fix delta at the same fixture blows past 200 MB
(bucket = 128 × 100k × 16 B). Bit-identity between parallel
(`threshold = 1L`) and serial (`threshold = .Machine$integer.max`)
dispatch holds for all six kernels on the 2k × 2k × 0.02 fixture.

## 2. Out of scope

- Acc-struct slimming / flat-storage refactor for mode and quantile
  per-cell overhead. Orthogonal constant-factor work; 9d-M deferred
  it and 9d-N keeps that deferral.
- mmap S7-ctor floor (4 accept-class breaches from 9c / 9d-M exits).
  Architectural, separate slice.
- `copy_all` double-write bug.
- 9d-M code-review deferred items:
  - `src/kernel_grouped_reduce_csc.cpp:34` — unused
    `using dafr_grouped::acc_merge;`
  - `src/kernel_grouped_mode_csc.cpp:203-204` — redundant
    `std::sort` (entries arrive pre-sorted).

  Both are housekeeping on 9d-M files; widening 9d-N's surface area
  for them yields no scope benefit. Separate cleanup slice.
- Dynamic OpenMP scheduling. Kickoff §"Decision point 5" lets this
  slide unless post-9d-M profiling on real skewed metacell data
  shows load imbalance. It does not, as of 9d-M exit. `schedule(static)`
  (inherited from the shim) stays.
- No new user-facing options. `dafr.kernel_threshold` remains the
  single parallel-dispatch lever.
- Row-partition helper abstraction. Each kernel has a different
  payload shape (double+double for var, double+int for minmax,
  `vector<double>` for quantile, `vector<Entry>` for mode) and the
  9d-M exit confirmed inline per-kernel edits are reviewable and
  debuggable; a shared template would need 5 instantiation shapes
  and hide invariants. Inline stays.

## 3. Locked decisions

1. **Scope: 6A bundled.** All six kernels in one slice. Dispatch
   audit in `R/query_eval.R` confirms every target is a live
   dispatch path — none are dead code. Leaving any unfixed caps
   the memory ceiling for parallel dispatch (kickoff "Known
   mines" §3).
2. **Template: 9d-M row-partition, inlined per kernel.** No shared
   helper.
3. **Scheduling:** static (inherited from `openmp_shim.h`).
4. **Two stress-test fixtures:**
   - **Bit-identity:** `nr = 2000, nc = 2000, density = 0.02`,
     all six kernels at axis = 0,
     `expect_identical(par = threshold 1L, ser = threshold INT_MAX)`.
   - **Peak-RSS:** `nr = 100000, nc = 5000, density = 0.02`
     (~10M nnz, ~160 MB input), one representative category-A
     kernel (`kernel_var_csc` with variant = `"Var"`),
     `bench::bench_process_memory()` delta budget: 100 MB.
5. **Profile before + after** on the same synthetic fixture at
   128 threads, numbers go in the exit note. Follows 9d-M.
6. **Pre-design profile baseline is run and reported** — see §8.
7. **No new user-facing options.**

## 4. Algorithm — row-partition axis = 0

All six kernels share the same structural transform:

```cpp
// axis == 0: per-row output.
// Row-partition: each thread owns a disjoint row range [r0, r1).
// Writes to shared-accumulator[r] are race-free because slot
// ownership is fixed by r. No thread buckets, no serial merge.
cpp11::writable::doubles out(nrow);
double *pout = REAL(out.data());
// ... initialise per-row accumulators to neutral element ...

DAFR_OMP_PARALLEL_IF(nrow >= threshold)
{
    const int tid = dafr_omp_get_thread_num();
    const int nt  = dafr_omp_get_num_threads();
    const int chunk = (nrow + nt - 1) / nt;
    const int r0 = std::min(nrow, tid * chunk);
    const int r1 = std::min(nrow, r0 + chunk);

    // Pass 1: scan every column, filter by row-range.
    for (int j = 0; j < ncol; ++j) {
        const int k_end = pp[j + 1];
        for (int k = pp[j]; k < k_end; ++k) {
            const int r = pi[k];
            if (r < r0 || r >= r1) continue;
            // write directly into shared accumulator[r]
        }
    }

    // Pass 2: post-process rows in [r0, r1).
    for (int r = r0; r < r1; ++r) {
        // derive per-row output from accumulator[r]
    }
}
return out;
```

### 4.1 `kernel_var_csc.cpp` (axis == 0)

- Pre-fix: `tsx`, `tsxx` (`nthreads × nrow` doubles each) + serial
  merge into `sx_tot`, `sxx_tot`.
- Post-fix: single shared `sx_tot`, `sxx_tot` (`nrow` doubles
  each), row-range-filtered writes.
- Pass 2 (post-process) inlined into the same parallel region:
  for `r in [r0, r1)`, compute `mean = sx_tot[r] / ncol`,
  `var = sxx_tot[r] / ncol - mean²`, write `pout[r]`.

### 4.2 `kernel_minmax_csc.cpp` (axis == 0)

- Pre-fix: `tbuf` (`nthreads × nrow` doubles, init sentinel) +
  `tnnz` (`nthreads × nrow` ints) + serial merge into `pout` and
  `nnz_per_row`.
- Post-fix: `pout` (init sentinel, `nrow` doubles), `nnz_per_row`
  (`nrow` ints), row-range-filtered writes.
- Pass 2: for `r in [r0, r1)`, fold implicit zero if
  `nnz_per_row[r] < ncol`.
- **Bit-identity note:** `min`/`max` are associative and
  commutative — result is order-independent. Trivially identical
  between serial and parallel dispatch.

### 4.3 `kernel_log_reduce.cpp` (axis == 0)

- Pre-fix: `tsum` (`nthreads × nrow` doubles) + `tnnz`
  (`nthreads × nrow` ints) + serial merge into `pout` (init 0)
  and `nnz_per_row`.
- Post-fix: `pout` (init 0), `nnz_per_row` (init 0), row-range-
  filtered writes.
- Pass 2: for `r in [r0, r1)`, add zero contribution
  `(ncol - nnz_per_row[r]) * log(eps)/log(base)` and divide by
  `ncol` if Mean.
- **Bit-identity note:** `pout[r]` receives contributions in
  column-ascending order by the single owning thread, identical
  to serial dispatch.

### 4.4 `kernel_geomean_csc.cpp` (axis == 0)

- Pre-fix: `tsum` (`nthreads × nrow` doubles) + `tnnz` (ints) +
  serial merge.
- Post-fix: `pout` (init 0), `nnz_per_row` (init 0), row-range-
  filtered writes.
- Pass 2: for `r in [r0, r1)`, add zero contribution
  `(ncol - nnz_per_row[r]) * log(eps)` if `has_eps`, then
  `pout[r] = exp(s / ncol) [- eps]`.
- **Bit-identity note:** same column-ascending argument as §4.3.

### 4.5 `kernel_mode_csc.cpp` (axis == 0)

- Pre-fix: serial fill of `std::vector<std::vector<Entry>> rows`
  (comment: none, but the race is implicit — `rows[pi[k]]` would
  race under column-parallelism).
- Post-fix: same `rows` vector, but fill pass is
  `DAFR_OMP_PARALLEL_IF(nrow >= threshold)` with row-range filter
  on `pi[k]`.
- **Bit-identity note:** mode semantics are "first column wins
  ties." Post-fix, the single thread owning row `r` scans
  `j = 0..ncol-1` in order and pushes entries in column-ascending
  order — identical to the serial fill. ✓
- Pass 2 (existing parallel post-process) stays; we can keep it as
  a separate `DAFR_PARALLEL_FOR` after the fill-pass parallel
  region, or fold it inside. 9d-M's `kernel_grouped_mode_csc` uses
  the fold-inside pattern. We adopt the same.

### 4.6 `kernel_quantile_csc.cpp` (axis == 0)

- Pre-fix: serial fill of `std::vector<std::vector<double>> rows`
  (with the explicit race comment).
- Post-fix: same `rows` vector, row-range-filtered fill pass.
- **Bit-identity note:** quantile sorts values internally
  (pick_rank partitions into neg / zero / pos) — push order is
  irrelevant. ✓
- Pass 2 (parallel post-process over rows) folds into the same
  parallel region.

## 5. Files touched

**New files:**
- `tests/testthat/test-kernel-csc-axis0-memory.R` — bit-identity
  guard (2k × 2k fixture, all six kernels) + peak-RSS guard
  (100k × 5k fixture, `kernel_var_csc`).

**Modified files:**
- `src/kernel_var_csc.cpp` — rewrite axis == 0 branch.
- `src/kernel_minmax_csc.cpp` — rewrite axis == 0 branch.
- `src/kernel_log_reduce.cpp` — rewrite axis == 0 branch.
- `src/kernel_geomean_csc.cpp` — rewrite axis == 0 branch.
- `src/kernel_mode_csc.cpp` — parallelise axis == 0 fill pass.
- `src/kernel_quantile_csc.cpp` — parallelise axis == 0 fill pass.
- `NEWS.md` — one-bullet entry under the unreleased section.

**Unchanged:**
- `src/cpp11.cpp`, `R/cpp11.R` — no C++ signature changes.
- `R/query_eval.R` — dispatch sites untouched.
- `R/options.R` — no new option.
- `src/openmp_shim.h` — already exposes
  `dafr_omp_get_num_threads()` and `DAFR_OMP_PARALLEL_IF` from
  9d-M.
- `axis == 1` branches of all six kernels.

## 6. Bit-identity argument

Four category-A kernels do floating-point sums whose numeric
result depends on summation order. `min`/`max` (category-A
`kernel_minmax_csc`) is order-independent.

**Serial dispatch (`threshold = .Machine$integer.max`):**
`DAFR_PARALLEL_FOR(ncol >= threshold)` evaluates false. A single
thread scans columns 0..ncol-1 in order, accumulating into its
thread-local bucket. The serial merge copies bucket[0] into the
shared accumulator. Result: values accumulate in column-
ascending order, per row.

**Parallel dispatch post-fix (`threshold = 1L`, 128 threads):**
Each row `r` is owned by exactly one thread. That thread scans
columns 0..ncol-1 in order and accumulates into the shared
`accumulator[r]`. Result: values accumulate in column-ascending
order, per row.

The two paths accumulate **the same values in the same order**
per row, therefore bit-identical. The test
`expect_identical(par, ser)` is a forward-looking invariant —
any refactor that breaks it (e.g., a future "clever" chunked
column assignment) will be caught.

**Parallel dispatch pre-fix** would NOT have been bit-identical
to serial: chunked column assignment put columns
`[t*C, (t+1)*C)` into `tsum[t]`, and the serial merge summed
buckets in thread order. This slice does not test pre-fix — the
invariant only needs to hold going forward.

Category-B kernels don't do numeric reduction during fill —
they push values into row-vectors. Mode's tie-breaking needs
column-ascending insertion order (which post-fix preserves,
§4.5). Quantile sorts internally so order doesn't matter.

## 7. Test plan

### 7.1 Bit-identity guard — `tests/testthat/test-kernel-csc-axis0-memory.R` (first `test_that`)

```r
skip_on_cran()
set.seed(42L)
nr <- 2000L; nc <- 2000L
nnz <- as.integer(nr * nc * 0.02)
m <- Matrix::sparseMatrix(
    i = sample.int(nr, nnz, replace = TRUE),
    j = sample.int(nc, nnz, replace = TRUE),
    x = runif(nnz, 0.1, 10.0),
    dims = c(nr, nc),
    repr = "C"
)

# kernel_var_csc — Var, Std, VarN, StdN all go through same axis-0 path
for (variant in c("Var", "Std", "VarN", "StdN")) {
    par <- kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                              axis = 0L, variant = variant, eps = 1e-6,
                              threshold = 1L)
    ser <- kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                              axis = 0L, variant = variant, eps = 1e-6,
                              threshold = .Machine$integer.max)
    expect_identical(par, ser)
}

# kernel_minmax_csc — Min, Max
for (variant in c("Min", "Max")) { ... expect_identical ... }

# kernel_log_reduce — Sum, Mean
for (reducer in c("Sum", "Mean")) { ... expect_identical ... }

# kernel_geomean_csc — both eps = 0 and eps > 0
for (eps in c(0, 1e-6)) { ... expect_identical ... }

# kernel_mode_csc
par_mode <- kernel_mode_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                axis = 0L, threshold = 1L)
ser_mode <- kernel_mode_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                axis = 0L, threshold = .Machine$integer.max)
expect_identical(par_mode, ser_mode)

# kernel_quantile_csc — q = 0.5 is sufficient (sort is dominant cost)
par_q <- kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                 axis = 0L, q = 0.5, threshold = 1L)
ser_q <- kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                 axis = 0L, q = 0.5,
                                 threshold = .Machine$integer.max)
expect_identical(par_q, ser_q)
```

### 7.2 Peak-RSS budget — `tests/testthat/test-kernel-csc-axis0-memory.R` (second `test_that`)

```r
skip_on_cran()
skip_if_not_installed("bench")
set.seed(42L)
nr <- 100000L; nc <- 5000L
nnz <- as.integer(nr * nc * 0.02)   # 10M nnz
m <- Matrix::sparseMatrix(...)

gc(full = TRUE)
mem_before <- bench::bench_process_memory()
out <- kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                          axis = 0L, variant = "Var", eps = 0,
                          threshold = 1L)
mem_after <- bench::bench_process_memory()
delta <- as.numeric(mem_after["max"]) - as.numeric(mem_before["max"])

# Budget: 100 MB. Post-fix: 2 * nrow * 8 B = 1.6 MB shared accs.
# Pre-fix at 128 threads: 128 * nrow * 16 B = 200 MB bucket — fails.
expect_lt(delta, 100 * 1024 * 1024)
expect_equal(length(out), nr)
expect_true(all(is.finite(out)))
```

### 7.3 Regression net

Full test suite (`testthat::test_package("dafr")`) must remain
green — expected pass count after 9d-N ≥ 1914 (1907 from 9d-M
+ the 2 new `test_that` blocks in the new file, which each
bundle multiple kernel assertions).

## 8. Pre-fix profile baseline

Run script `dev/scripts/stress-9d-n-memory.R` on current
`main` (commit `676386e`, before any 9d-N edits):

```r
# One representative query per kernel, at 128 threads and serial.
# Report: wall-time, peak RSS (via /usr/bin/time -v externally).
# Fixture: nr = 100000, nc = 5000, density 0.02, seed 42.
```

Expected numbers (from kickoff estimates; actuals recorded post-
run):
- `kernel_var_csc`: 128-thread RSS delta ≈ 200 MB (bucket) vs.
  1.6 MB post-fix.
- Same pattern for minmax, log_reduce, geomean.
- `kernel_mode_csc`, `kernel_quantile_csc`: serial fill cost
  ~20–50 ms pre-fix; parallel post-fix scales with cores.

Numbers written to `dev/notes/slice-9d-n-profile.md`.

## 9. Branch, commits, merge

Working branch: `slice-9d-n-csc-axis0-memory-fix`, cut from tag
`slice-9d-m` (commit `676386e`).

Planned commits:
1. `perf(9d-n): row-partition axis=0 branch of kernel_var_csc`
2. `perf(9d-n): row-partition axis=0 branch of kernel_minmax_csc`
3. `perf(9d-n): row-partition axis=0 branch of kernel_log_reduce`
4. `perf(9d-n): row-partition axis=0 branch of kernel_geomean_csc`
5. `perf(9d-n): parallelise axis=0 fill of kernel_mode_csc`
6. `perf(9d-n): parallelise axis=0 fill of kernel_quantile_csc`
7. `test(9d-n): regression guards for CSC axis=0 row-partition`
8. `docs(9d-n): NEWS entry for CSC axis=0 row-partition sweep`
9. Merge commit into `main` with message
   `merge: slice 9d-n — CSC axis=0 thread-bucket memory fix`.

Each perf commit runs the full test suite locally before the
next one lands. Per-kernel isolation means a failed suite =
revert one commit, not the whole slice. Bundled PR / merge
strategy follows 9d-M.

Tag: `slice-9d-n` on the merge commit.

## 10. Open questions

None at spec-lock. All kickoff decision points (scope, fixture
shape, profile before/after, helper vs inline, schedule clause)
are resolved and captured in §3.
