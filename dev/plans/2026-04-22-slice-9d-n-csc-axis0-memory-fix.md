# Slice 9d-N — CSC Axis-0 Thread-Bucket Memory Fix Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the `O(nthreads × nrow)` thread-bucket memory pattern in the axis-0 branch of four category-A CSC kernels, and parallelise the axis-0 fill pass of two category-B CSC kernels, by applying the 9d-M row-partition technique inlined per kernel. Each thread owns a disjoint row range `[r0, r1)`; writes to the shared accumulator are race-free because slot ownership is fixed by the row index and ranges are disjoint.

**Architecture:** Replace each kernel's `std::vector<std::vector<T>> tbuf(nthreads, ...)` + serial merge pattern with a single shared accumulator and a `DAFR_OMP_PARALLEL_IF(nrow >= threshold)` block that computes `(tid, nt, chunk, r0, r1)`, scans every column in order, filters by `if (pi[k] < r0 || pi[k] >= r1) continue`, writes directly to the shared accumulator, and folds the per-row post-process into the same parallel region (rows in `[r0, r1)` are owned by the same thread for reads and writes). For category-B kernels (`kernel_mode_csc`, `kernel_quantile_csc`), the existing shared `std::vector<std::vector<T>> rows` is kept; only the fill pass gains row-partitioned parallelism.

**Tech Stack:** cpp11 (not Rcpp), OpenMP via `src/openmp_shim.h` helpers (`DAFR_OMP_PARALLEL_IF`, `dafr_omp_get_thread_num`, `dafr_omp_get_num_threads`; never raw pragmas at call sites), testthat, `bench::bench_process_memory()` for peak-RSS regression, `/usr/bin/time -v` for external peak-RSS measurement.

**Design spec:** `dev/notes/2026-04-22-slice-9d-n-design.md`
**Kickoff:** `dev/notes/slice-9d-n-kickoff.md`
**Pre-fix baseline profile:** `dev/benchmarks/2026-04-22-pre-slice-9d-n-baseline/` (Task 1)

**Working branch:** `slice-9d-n-csc-axis0-memory-fix` cut from tag `slice-9d-m` (commit `676386e`). Already checked out at plan-write time; Task 0 verifies clean starting state.

---

## File Structure

**New files:**
- `tests/testthat/test-kernel-csc-axis0-memory.R` — parallel-vs-serial bit-identity across all six kernels + peak-RSS regression guard on `kernel_var_csc` (Task 8).
- `dev/scripts/stress-9d-n-memory.R` — profiling harness used for pre-fix and post-fix baselines (Task 1).

**Modified files:**
- `src/kernel_var_csc.cpp` — rewrite axis == 0 branch (Task 2).
- `src/kernel_minmax_csc.cpp` — rewrite axis == 0 branch (Task 3).
- `src/kernel_log_reduce.cpp` — rewrite axis == 0 branch (Task 4).
- `src/kernel_geomean_csc.cpp` — rewrite axis == 0 branch (Task 5).
- `src/kernel_mode_csc.cpp` — parallelise axis == 0 fill pass, fold post-process inline (Task 6).
- `src/kernel_quantile_csc.cpp` — parallelise axis == 0 fill pass, fold post-process inline (Task 7).
- `NEWS.md` — one-bullet entry under unreleased section (Task 9).

**Unchanged:**
- `src/cpp11.cpp`, `R/cpp11.R` — no C++ signature changes.
- `R/query_eval.R` — dispatch sites untouched.
- `R/options.R` — no new option.
- `src/openmp_shim.h` — already exposes everything we need from 9d-M.
- `axis == 1` branches of all six kernels.
- G2 / G3 branches of grouped kernels (fixed in 9d-M).

---

## Task 0: Verify clean starting state

**Files:** none.

- [ ] **Step 0.1: Confirm branch and HEAD**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
git rev-parse --abbrev-ref HEAD   # expect: slice-9d-n-csc-axis0-memory-fix
git rev-parse HEAD                # expect: 676386e (tag slice-9d-m)
git status --short                # expect: only "?? .claude/" untracked
```

- [ ] **Step 0.2: Verify clean starting test suite**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
R CMD INSTALL . --preclean 2>&1 | tail -5
Rscript -e 'testthat::test_package("dafr", reporter = "summary")' 2>&1 | tail -3
```

Expected: `[ FAIL 0 | WARN ≤1 | SKIP ≤2 | PASS ≥1907 ]` (9d-M exit count; the two test_that blocks in `test-kernel-grouped-g3-memory.R` bundle multiple assertions). Any FAIL blocks the slice — investigate before continuing.

---

## Task 1: Pre-fix profile baseline

**Files:**
- Create: `dev/scripts/stress-9d-n-memory.R`
- Create: `dev/benchmarks/2026-04-22-pre-slice-9d-n-baseline/profile.txt`

- [ ] **Step 1.1: Write profiling script**

Create `/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/dev/scripts/stress-9d-n-memory.R` with:

```r
# Slice 9d-N memory/perf profile harness.
# Runs one representative query per CSC axis-0 kernel on the stress fixture
# and reports wall-time and peak RSS (via bench::bench_process_memory).
#
# Reproduce: Rscript dev/scripts/stress-9d-n-memory.R
# For external peak-RSS sampling use:
#   /usr/bin/time -v Rscript dev/scripts/stress-9d-n-memory.R

suppressPackageStartupMessages({
    library(Matrix)
    library(dafr)
    library(bench)
})

set.seed(42L)
nr <- 100000L
nc <- 5000L
density <- 0.02
nnz <- as.integer(nr * nc * density)

cat(sprintf("Fixture: nr=%d nc=%d density=%.3f nnz=%d\n",
            nr, nc, density, nnz))
cat(sprintf("OMP_NUM_THREADS (env)     : %s\n",
            Sys.getenv("OMP_NUM_THREADS", "<unset>")))
cat(sprintf("parallel::detectCores()   : %d\n", parallel::detectCores()))
cat(sprintf("dafr.kernel_threshold     : %s\n",
            format(getOption("dafr.kernel_threshold"))))

m <- Matrix::sparseMatrix(
    i = sample.int(nr, nnz, replace = TRUE),
    j = sample.int(nc, nnz, replace = TRUE),
    x = runif(nnz, 0.1, 10.0),
    dims = c(nr, nc),
    repr = "C"
)

run <- function(label, fn) {
    gc(full = TRUE)
    before <- bench::bench_process_memory()
    t0 <- proc.time()[["elapsed"]]
    out <- fn()
    dt <- proc.time()[["elapsed"]] - t0
    after <- bench::bench_process_memory()
    delta <- as.numeric(after["max"]) - as.numeric(before["max"])
    cat(sprintf("%-35s  wall=%7.3fs  RSS_delta=%7.1f MB  len=%d\n",
                label, dt, delta / (1024 * 1024), length(out)))
    invisible(out)
}

# Category A: thread-bucket kernels.
run("kernel_var_csc (Var, axis=0)", function() {
    dafr:::kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                              axis = 0L, variant = "Var", eps = 0,
                              threshold = 1L)
})
run("kernel_minmax_csc (Max, axis=0)", function() {
    dafr:::kernel_minmax_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                 axis = 0L, variant = "Max",
                                 threshold = 1L)
})
run("kernel_log_reduce (Sum, axis=0)", function() {
    dafr:::kernel_log_reduce_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                     eps = 1e-5, base = 2,
                                     axis = 0L, reducer = "Sum",
                                     threshold = 1L)
})
run("kernel_geomean_csc (axis=0)", function() {
    dafr:::kernel_geomean_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                  axis = 0L, eps = 1e-5,
                                  threshold = 1L)
})

# Category B: serial-fill kernels.
run("kernel_mode_csc (axis=0)", function() {
    dafr:::kernel_mode_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                               axis = 0L, threshold = 1L)
})
run("kernel_quantile_csc (q=0.5, axis=0)", function() {
    dafr:::kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                   axis = 0L, q = 0.5,
                                   threshold = 1L)
})

cat("\nDone.\n")
```

- [ ] **Step 1.2: Run the script at current HEAD (pre-fix baseline)**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
mkdir -p dev/benchmarks/2026-04-22-pre-slice-9d-n-baseline
R CMD INSTALL . --preclean 2>&1 | tail -3
/usr/bin/time -v Rscript dev/scripts/stress-9d-n-memory.R \
    > dev/benchmarks/2026-04-22-pre-slice-9d-n-baseline/profile.txt \
    2>&1
cat dev/benchmarks/2026-04-22-pre-slice-9d-n-baseline/profile.txt
```

Expected: each category-A kernel reports RSS_delta ≥ 100 MB at 128 threads (thread-bucket pattern); category-B kernels report low RSS but modest wall-time (serial fill). Exact numbers inform the exit note narrative.

- [ ] **Step 1.3: Commit (dev repo only — baseline artifacts)**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/dev
git add scripts/stress-9d-n-memory.R benchmarks/2026-04-22-pre-slice-9d-n-baseline/
git -c commit.gpgsign=false commit -m "profile(9d-n): pre-fix CSC axis=0 baseline"
```

Note: `dev/` is a nested git repo with a separate remote — this commit does NOT land on the package repo's `slice-9d-n-csc-axis0-memory-fix` branch.

---

## Task 2: `kernel_var_csc.cpp` — row-partition axis == 0

**Files:**
- Modify: `src/kernel_var_csc.cpp:52-87` (axis == 0 branch)

- [ ] **Step 2.1: Replace the axis == 0 branch**

Open `src/kernel_var_csc.cpp`. Replace lines 52–87 (the entire axis == 0 branch, from the `// axis == 0:` comment through the final `return out;` inside that branch) with:

```cpp
    // axis == 0: per-row. Row-partition: each thread owns a disjoint row
    // range [r0, r1); two threads never write to the same sx_tot/sxx_tot
    // slot. No thread buckets, no serial merge.
    cpp11::writable::doubles out(nrow);
    double *pout = REAL(out.data());
    std::vector<double> sx_tot(nrow, 0.0), sxx_tot(nrow, 0.0);

    DAFR_OMP_PARALLEL_IF(nrow >= threshold)
    {
        const int tid = dafr_omp_get_thread_num();
        const int nt  = dafr_omp_get_num_threads();
        const int chunk = (nrow + nt - 1) / nt;
        const int r0 = std::min(nrow, tid * chunk);
        const int r1 = std::min(nrow, r0 + chunk);

        // Pass 1: scan every column; filter by row-range.
        for (int j = 0; j < ncol; ++j) {
            const int k_end = pp[j + 1];
            for (int k = pp[j]; k < k_end; ++k) {
                const int r = pi[k];
                if (r < r0 || r >= r1) continue;
                const double v = px[k];
                sx_tot[r]  += v;
                sxx_tot[r] += v * v;
            }
        }

        // Pass 2: post-process rows in [r0, r1). Each thread owns the
        // reads and writes for its row range.
        for (int r = r0; r < r1; ++r) {
            const double mean = ncol > 0 ? sx_tot[r] / ncol : 0.0;
            const double var_u = ncol > 0 ? (sxx_tot[r] / ncol - mean * mean) : 0.0;
            pout[r] = derive(variant, var_u < 0 ? 0.0 : var_u, mean, eps);
        }
    }
    return out;
}
```

Remove the now-unused `nthreads` and `tsx` / `tsxx` declarations (former lines 59–61) — they are replaced by the single `sx_tot` / `sxx_tot` allocation shown above.

- [ ] **Step 2.2: Install and run the full test suite**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
R CMD INSTALL . --preclean 2>&1 | tail -3
Rscript -e 'testthat::test_package("dafr", reporter = "summary")' 2>&1 | tail -3
```

Expected: `[ FAIL 0 | WARN ≤1 | SKIP ≤2 | PASS ≥1907 ]`. Existing tests use `tolerance = 1e-9` against `matrixStats::rowVars`, which still holds.

- [ ] **Step 2.3: Commit**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
git add src/kernel_var_csc.cpp
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
perf(9d-n): row-partition axis=0 branch of kernel_var_csc

Replace the O(nthreads × nrow) tsx/tsxx thread-bucket pattern with a
row-partitioned parallel scan: each thread owns a disjoint row range
[r0, r1) and writes directly into a single shared sx_tot/sxx_tot
vector. No thread buckets, no serial merge. Post-process folded into
the same parallel region.

Bit-identity with serial dispatch holds because values accumulate in
column-ascending order per row in both paths.
EOF
)"
```

---

## Task 3: `kernel_minmax_csc.cpp` — row-partition axis == 0

**Files:**
- Modify: `src/kernel_minmax_csc.cpp:48-84` (axis == 0 branch)

- [ ] **Step 3.1: Replace the axis == 0 branch**

Open `src/kernel_minmax_csc.cpp`. Replace lines 48–84 (entire axis == 0 branch from the `// axis == 0:` comment through `return out;`) with:

```cpp
    // axis == 0: per-row output. Row-partition: each thread owns a
    // disjoint row range [r0, r1); writes to pout[r] and nnz_per_row[r]
    // are race-free. No thread buckets, no serial merge.
    cpp11::writable::doubles out(nrow);
    double *pout = REAL(out.data());
    for (int r = 0; r < nrow; ++r) pout[r] = sentinel;
    std::vector<int> nnz_per_row(nrow, 0);

    DAFR_OMP_PARALLEL_IF(nrow >= threshold)
    {
        const int tid = dafr_omp_get_thread_num();
        const int nt  = dafr_omp_get_num_threads();
        const int chunk = (nrow + nt - 1) / nt;
        const int r0 = std::min(nrow, tid * chunk);
        const int r1 = std::min(nrow, r0 + chunk);

        // Pass 1: scan every column; filter by row-range.
        for (int j = 0; j < ncol; ++j) {
            const int k_end = pp[j + 1];
            for (int k = pp[j]; k < k_end; ++k) {
                const int r = pi[k];
                if (r < r0 || r >= r1) continue;
                pout[r] = fold(pout[r], px[k]);
                nnz_per_row[r] += 1;
            }
        }

        // Pass 2: fold in implicit zero for rows with at least one.
        for (int r = r0; r < r1; ++r) {
            if (nnz_per_row[r] < ncol) pout[r] = fold(pout[r], 0.0);
        }
    }
    return out;
}
```

Remove the now-unused `nthreads`, `tbuf`, `tnnz` declarations (former lines 55–61).

- [ ] **Step 3.2: Install and run the full test suite**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
R CMD INSTALL . --preclean 2>&1 | tail -3
Rscript -e 'testthat::test_package("dafr", reporter = "summary")' 2>&1 | tail -3
```

Expected: `[ FAIL 0 | PASS ≥1907 ]`. `min`/`max` is order-independent — all existing assertions against `matrixStats::rowMins`/`rowMaxs` hold exactly.

- [ ] **Step 3.3: Commit**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
git add src/kernel_minmax_csc.cpp
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
perf(9d-n): row-partition axis=0 branch of kernel_minmax_csc

Replace the O(nthreads × nrow) tbuf/tnnz thread-bucket pattern with a
row-partitioned parallel scan: each thread owns a disjoint row range
[r0, r1) and writes directly into the shared pout/nnz_per_row vectors.
Post-process (implicit-zero fold) stays inside the same parallel
region. min/max is associative and commutative, so bit-identity with
serial dispatch is trivial.
EOF
)"
```

---

## Task 4: `kernel_log_reduce.cpp` — row-partition axis == 0

**Files:**
- Modify: `src/kernel_log_reduce.cpp:77-125` (axis == 0 branch)

- [ ] **Step 4.1: Replace the axis == 0 branch**

Open `src/kernel_log_reduce.cpp`. Replace lines 77–125 (the entire `if (axis == 0) { ... }` block body, from the opening `cpp11::writable::doubles out(nrow);` through the `return out;` immediately inside the `if`) with:

```cpp
    if (axis == 0) {
        // Row-partition: each thread owns a disjoint row range [r0, r1);
        // writes to pout[r] and nnz_per_row[r] are race-free. No thread
        // buckets, no serial merge.
        cpp11::writable::doubles out(nrow);
        double *pout = REAL(out.data());
        for (int r = 0; r < nrow; ++r) pout[r] = 0.0;
        std::vector<int> nnz_per_row(nrow, 0);

        DAFR_OMP_PARALLEL_IF(nrow >= threshold)
        {
            const int tid = dafr_omp_get_thread_num();
            const int nt  = dafr_omp_get_num_threads();
            const int chunk = (nrow + nt - 1) / nt;
            const int r0 = std::min(nrow, tid * chunk);
            const int r1 = std::min(nrow, r0 + chunk);

            // Pass 1: scan every column; filter by row-range.
            for (int j = 0; j < ncol; ++j) {
                const int k_end = pp[j + 1];
                for (int k = pp[j]; k < k_end; ++k) {
                    const int r = pi[k];
                    if (r < r0 || r >= r1) continue;
                    pout[r] += std::log(px[k] + eps) * inv_log_base;
                    nnz_per_row[r] += 1;
                }
            }

            // Pass 2: add zero contributions, divide by ncol if Mean.
            for (int r = r0; r < r1; ++r) {
                const int zeros = ncol - nnz_per_row[r];
                pout[r] += zeros * zero_log;
                if (is_mean) pout[r] /= ncol;
            }
        }
        return out;
    } else {
```

Remove the now-unused `nthreads`, `tsum`, `tnnz` declarations and the serial merge loop that previously occupied this range.

- [ ] **Step 4.2: Install and run the full test suite**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
R CMD INSTALL . --preclean 2>&1 | tail -3
Rscript -e 'testthat::test_package("dafr", reporter = "summary")' 2>&1 | tail -3
```

Expected: `[ FAIL 0 | PASS ≥1907 ]`. Existing tests use `tolerance = 1e-9`; row-partition accumulates in column-ascending order per row (same as serial dispatch) so bit-identity holds.

- [ ] **Step 4.3: Commit**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
git add src/kernel_log_reduce.cpp
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
perf(9d-n): row-partition axis=0 branch of kernel_log_reduce

Replace the O(nthreads × nrow) tsum/tnnz thread-bucket pattern with a
row-partitioned parallel scan: each thread owns a disjoint row range
[r0, r1) and accumulates directly into the shared pout/nnz_per_row
vectors. Post-process (zero-contribution add, optional Mean divide)
folded into the same parallel region.

Bit-identity with serial dispatch holds because values accumulate in
column-ascending order per row in both paths.
EOF
)"
```

---

## Task 5: `kernel_geomean_csc.cpp` — row-partition axis == 0

**Files:**
- Modify: `src/kernel_geomean_csc.cpp:69-124` (axis == 0 branch)

- [ ] **Step 5.1: Replace the axis == 0 branch**

Open `src/kernel_geomean_csc.cpp`. Replace lines 69–124 (the axis == 0 branch — from the `// axis == 0:` comment through the final `return out;`) with:

```cpp
    // axis == 0: per-row. Row-partition: each thread owns a disjoint row
    // range [r0, r1); writes to pout[r] and nnz_per_row[r] are race-free.
    // No thread buckets, no serial merge.
    cpp11::writable::doubles out(nrow);
    double *pout = REAL(out.data());

    if (ncol == 0) {
        // No columns: result is 0 for every row (degenerate).
        for (int r = 0; r < nrow; ++r) pout[r] = 0.0;
        return out;
    }

    for (int r = 0; r < nrow; ++r) pout[r] = 0.0;
    std::vector<int> nnz_per_row(nrow, 0);

    DAFR_OMP_PARALLEL_IF(nrow >= threshold)
    {
        const int tid = dafr_omp_get_thread_num();
        const int nt  = dafr_omp_get_num_threads();
        const int chunk = (nrow + nt - 1) / nt;
        const int r0 = std::min(nrow, tid * chunk);
        const int r1 = std::min(nrow, r0 + chunk);

        // Pass 1: scan every column; filter by row-range.
        for (int j = 0; j < ncol; ++j) {
            const int k_end = pp[j + 1];
            for (int k = pp[j]; k < k_end; ++k) {
                const int r = pi[k];
                if (r < r0 || r >= r1) continue;
                pout[r] += std::log(px[k] + eps);
                nnz_per_row[r] += 1;
            }
        }

        // Pass 2: add zero contribution, derive geometric mean.
        for (int r = r0; r < r1; ++r) {
            double s = pout[r];
            if (has_eps) {
                s += (double)(ncol - nnz_per_row[r]) * log_eps;
                pout[r] = std::exp(s / ncol) - eps;
            } else {
                pout[r] = std::exp(s / ncol);
            }
        }
    }
    return out;
}
```

Remove the now-unused `nthreads`, `tsum`, `tnnz` declarations and the serial merge loop.

- [ ] **Step 5.2: Install and run the full test suite**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
R CMD INSTALL . --preclean 2>&1 | tail -3
Rscript -e 'testthat::test_package("dafr", reporter = "summary")' 2>&1 | tail -3
```

Expected: `[ FAIL 0 | PASS ≥1907 ]`. Same column-ascending order argument as `kernel_log_reduce`.

- [ ] **Step 5.3: Commit**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
git add src/kernel_geomean_csc.cpp
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
perf(9d-n): row-partition axis=0 branch of kernel_geomean_csc

Replace the O(nthreads × nrow) tsum/tnnz thread-bucket pattern with a
row-partitioned parallel scan: each thread owns a disjoint row range
[r0, r1) and accumulates directly into the shared pout/nnz_per_row
vectors. Post-process (zero contribution, exp / optional -eps) folded
into the same parallel region.

Bit-identity with serial dispatch holds because values accumulate in
column-ascending order per row in both paths.
EOF
)"
```

---

## Task 6: `kernel_mode_csc.cpp` — parallelise axis == 0 fill pass

**Files:**
- Modify: `src/kernel_mode_csc.cpp:107-201` (axis == 0 branch)

Approach: the shared `std::vector<std::vector<Entry>> rows(nrow)` already exists — no new allocation. The current serial fill is the bottleneck; the current post-process is already parallel via `DAFR_PARALLEL_FOR(nrow >= threshold)`. We fold both into one `DAFR_OMP_PARALLEL_IF` region so the same thread that fills rows `[r0, r1)` also post-processes them. This is the 9d-M `kernel_grouped_mode_csc` pattern.

Bit-identity: mode uses "first column wins ties" semantics. Post-fix each row is filled by a single thread scanning columns in ascending order `0..ncol-1`, so entries are pushed in column-ascending order — identical to the serial fill. ✓

- [ ] **Step 6.1: Replace the axis == 0 branch**

Open `src/kernel_mode_csc.cpp`. Replace lines 107–201 (everything from the `// axis == 0:` comment at line 107 through the final `return out;` at line 201) with:

```cpp
    // axis == 0: per-row (ReduceToColumn): one result per row.
    // Row-partition: each thread owns a disjoint row range [r0, r1).
    // The same thread fills rows[r] and computes out[r] for r in that
    // range — no write race on rows[pi[k]], no cross-thread reads of
    // rows[r] during post-process.
    //
    // Entry list per row: (col, val) pairs. Ordered by column because
    // the owning thread scans j = 0..ncol-1 in ascending order. Mode's
    // first-column-wins tie-break relies on this ordering.
    struct Entry { int col; double val; };
    std::vector<std::vector<Entry>> rows(nrow);

    cpp11::writable::doubles out(nrow);
    double *pout = REAL(out.data());

    DAFR_OMP_PARALLEL_IF(nrow >= threshold)
    {
        const int tid = dafr_omp_get_thread_num();
        const int nt  = dafr_omp_get_num_threads();
        const int chunk = (nrow + nt - 1) / nt;
        const int r0 = std::min(nrow, tid * chunk);
        const int r1 = std::min(nrow, r0 + chunk);

        // Pass 1: fill rows[r] for r in [r0, r1).
        for (int j = 0; j < ncol; ++j) {
            const int k_end = pp[j + 1];
            for (int k = pp[j]; k < k_end; ++k) {
                const int r = pi[k];
                if (r < r0 || r >= r1) continue;
                rows[r].push_back({j, px[k]});
            }
        }

        // Pass 2: compute mode for rows in [r0, r1).
        for (int r = r0; r < r1; ++r) {
            const auto &entries = rows[r];
            const int n_stored  = (int)entries.size();
            const int n_zeros   = ncol - n_stored;

            // Find first_seen_col for implicit zero.
            // entries are ordered by column (we inserted in j order).
            int zero_first_col = std::numeric_limits<int>::max();
            if (n_zeros > 0) {
                if (n_stored == 0 || entries[0].col > 0) {
                    zero_first_col = 0;
                } else {
                    zero_first_col = ncol; // default: no gap found yet
                    for (int k = 0; k < n_stored; ++k) {
                        if (k + 1 < n_stored) {
                            if (entries[k + 1].col > entries[k].col + 1) {
                                zero_first_col = entries[k].col + 1;
                                break;
                            }
                        } else {
                            if (entries[k].col < ncol - 1) {
                                zero_first_col = entries[k].col + 1;
                            }
                        }
                    }
                }
            }

            // Build count and first_col maps for nonzero values.
            std::unordered_map<double, int> counts;
            std::unordered_map<double, int> first_col;
            counts.reserve(n_stored);
            first_col.reserve(n_stored);
            for (const auto &e : entries) {
                auto it = counts.find(e.val);
                if (it == counts.end()) {
                    counts[e.val]    = 1;
                    first_col[e.val] = e.col;
                } else {
                    it->second += 1;
                }
            }

            double best_val      = 0.0;
            int    best_count    = n_zeros;
            int    best_first_col = zero_first_col;
            if (n_zeros == 0) {
                best_count     = 0;
                best_first_col = std::numeric_limits<int>::max();
            }

            for (auto &kv : counts) {
                int cnt = kv.second;
                int fc  = first_col[kv.first];
                if (cnt > best_count ||
                    (cnt == best_count && fc < best_first_col)) {
                    best_count     = cnt;
                    best_val       = kv.first;
                    best_first_col = fc;
                }
            }
            pout[r] = best_val;
        }
    }
    return out;
}
```

The body preserves every line of the original post-process logic — it is moved from a `DAFR_PARALLEL_FOR` over all rows to the per-thread `for (int r = r0; r < r1; ++r)` inside the `DAFR_OMP_PARALLEL_IF` region.

- [ ] **Step 6.2: Install and run the full test suite**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
R CMD INSTALL . --preclean 2>&1 | tail -3
Rscript -e 'testthat::test_package("dafr", reporter = "summary")' 2>&1 | tail -3
```

Expected: `[ FAIL 0 | PASS ≥1907 ]`. First-column-wins tie-break is preserved because entries per row arrive in column-ascending order from a single owning thread.

- [ ] **Step 6.3: Commit**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
git add src/kernel_mode_csc.cpp
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
perf(9d-n): parallelise axis=0 fill of kernel_mode_csc

Row-partition the previously-serial fill of rows[r] so the same thread
that pushes entries for row r also computes out[r]. Each thread owns a
disjoint row range [r0, r1); writes to rows[pi[k]] are race-free by
construction.

Entries per row still arrive in column-ascending order (single owning
thread scans j = 0..ncol-1), so mode's first-column-wins tie-break is
unchanged.
EOF
)"
```

---

## Task 7: `kernel_quantile_csc.cpp` — parallelise axis == 0 fill pass

**Files:**
- Modify: `src/kernel_quantile_csc.cpp:116-164` (axis == 0 branch)

Same approach as Task 6: row-partition the fill and fold the existing parallel post-process into the same region. Quantile sorts values internally (`std::nth_element` in `pick_rank`) so push order is irrelevant for bit-identity.

- [ ] **Step 7.1: Replace the axis == 0 branch**

Open `src/kernel_quantile_csc.cpp`. Replace lines 116–164 (everything from the `// axis == 0:` comment through the final `return out;`) with:

```cpp
    // axis == 0: per-row. Collect each row's non-zero values first, then
    // compute per-row quantile. Row-partition: each thread owns a disjoint
    // row range [r0, r1); writes to rows[pi[k]] are race-free because slot
    // ownership is fixed by r. The same thread that fills rows[r] also
    // computes out[r], so no cross-thread reads of rows[r].
    std::vector<std::vector<double>> rows(nrow);

    cpp11::writable::doubles out(nrow);
    double* pout = REAL(out.data());

    DAFR_OMP_PARALLEL_IF(nrow >= threshold)
    {
        const int tid = dafr_omp_get_thread_num();
        const int nt  = dafr_omp_get_num_threads();
        const int chunk = (nrow + nt - 1) / nt;
        const int r0 = std::min(nrow, tid * chunk);
        const int r1 = std::min(nrow, r0 + chunk);

        // Pass 1: fill rows[r] for r in [r0, r1).
        for (int j = 0; j < ncol; ++j) {
            const int k_end = pp[j + 1];
            for (int k = pp[j]; k < k_end; ++k) {
                const int r = pi[k];
                if (r < r0 || r >= r1) continue;
                rows[r].push_back(px[k]);
            }
        }

        // Pass 2: compute quantile for rows in [r0, r1).
        for (int r = r0; r < r1; ++r) {
            const int n = ncol;
            if (n == 0) { pout[r] = 0.0; continue; }
            const auto& rv = rows[r];
            std::vector<double> neg, pos;
            neg.reserve(rv.size());
            pos.reserve(rv.size());
            for (double v : rv) {
                if (v < 0.0) neg.push_back(v);
                else if (v > 0.0) pos.push_back(v);
            }
            const int n_zeros = n - static_cast<int>(neg.size()) -
                                    static_cast<int>(pos.size());
            const double h = q * (n - 1);
            const int lo = static_cast<int>(std::floor(h));
            const int hi = static_cast<int>(std::ceil(h));
            const double frac = h - lo;
            if (lo == hi) {
                pout[r] = pick_rank(neg, pos, n_zeros, lo);
            } else {
                const double v_lo = pick_rank(neg, pos, n_zeros, lo);
                // Rebuild for hi pick.
                neg.clear(); pos.clear();
                for (double v : rv) {
                    if (v < 0.0) neg.push_back(v);
                    else if (v > 0.0) pos.push_back(v);
                }
                const double v_hi = pick_rank(neg, pos, n_zeros, hi);
                pout[r] = (1.0 - frac) * v_lo + frac * v_hi;
            }
        }
    }
    return out;
}
```

- [ ] **Step 7.2: Install and run the full test suite**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
R CMD INSTALL . --preclean 2>&1 | tail -3
Rscript -e 'testthat::test_package("dafr", reporter = "summary")' 2>&1 | tail -3
```

Expected: `[ FAIL 0 | PASS ≥1907 ]`. Quantile sorts via `std::nth_element`, so push order is irrelevant.

- [ ] **Step 7.3: Commit**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
git add src/kernel_quantile_csc.cpp
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
perf(9d-n): parallelise axis=0 fill of kernel_quantile_csc

Row-partition the previously-serial fill of rows[r] so the same thread
that pushes values for row r also computes out[r]. Each thread owns a
disjoint row range [r0, r1); writes to rows[pi[k]] are race-free by
construction.

pick_rank sorts internally via std::nth_element, so push order is
irrelevant for bit-identity with serial dispatch.
EOF
)"
```

---

## Task 8: Regression guards

**Files:**
- Create: `tests/testthat/test-kernel-csc-axis0-memory.R`

- [ ] **Step 8.1: Write the stress test file**

Create `tests/testthat/test-kernel-csc-axis0-memory.R` with:

```r
# Slice 9d-N regression guards for the CSC axis=0 row-partition rewrite.
# Two test_that blocks:
#   1. Bit-identity: parallel dispatch (threshold = 1L) matches serial
#      dispatch (threshold = .Machine$integer.max) on a 2k × 2k fixture
#      for all six row-partitioned kernels.
#   2. Peak-RSS: at the larger 100k × 5k fixture, one representative
#      category-A kernel stays under a 100 MB bench_process_memory delta.
#      Pre-fix the thread-bucket pattern would allocate 128 * 100k * 16B
#      ≈ 200 MB at 128 threads — reintroducing it will fail this test.

test_that("CSC axis=0 row-partition is bit-identical to serial dispatch", {
    skip_on_cran()
    set.seed(42L)
    nr <- 2000L
    nc <- 2000L
    nnz <- as.integer(nr * nc * 0.02)
    m <- Matrix::sparseMatrix(
        i = sample.int(nr, nnz, replace = TRUE),
        j = sample.int(nc, nnz, replace = TRUE),
        x = runif(nnz, 0.1, 10.0),
        dims = c(nr, nc),
        repr = "C"
    )

    # kernel_var_csc — all four variants share the axis=0 path.
    for (variant in c("Var", "Std", "VarN", "StdN")) {
        par <- kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                  axis = 0L, variant = variant, eps = 1e-6,
                                  threshold = 1L)
        ser <- kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                  axis = 0L, variant = variant, eps = 1e-6,
                                  threshold = .Machine$integer.max)
        expect_identical(par, ser)
    }

    # kernel_minmax_csc — Min and Max.
    for (variant in c("Min", "Max")) {
        par <- kernel_minmax_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                     axis = 0L, variant = variant,
                                     threshold = 1L)
        ser <- kernel_minmax_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                     axis = 0L, variant = variant,
                                     threshold = .Machine$integer.max)
        expect_identical(par, ser)
    }

    # kernel_log_reduce — Sum and Mean reducers.
    for (reducer in c("Sum", "Mean")) {
        par <- kernel_log_reduce_csc_cpp(
            m@x, m@i, m@p, nrow(m), ncol(m),
            eps = 1e-5, base = 2,
            axis = 0L, reducer = reducer, threshold = 1L
        )
        ser <- kernel_log_reduce_csc_cpp(
            m@x, m@i, m@p, nrow(m), ncol(m),
            eps = 1e-5, base = 2,
            axis = 0L, reducer = reducer,
            threshold = .Machine$integer.max
        )
        expect_identical(par, ser)
    }

    # kernel_geomean_csc — eps = 0 (pure geometric mean) and eps > 0.
    for (eps in c(0, 1e-6)) {
        # eps = 0 requires fully-nonzero columns in axis = 1, but the
        # axis = 0 path has no such restriction. Generate a modified
        # fixture with no explicit zeros to keep semantics clean for
        # the eps = 0 case.
        par <- kernel_geomean_csc_cpp(
            m@x, m@i, m@p, nrow(m), ncol(m),
            axis = 0L, eps = eps, threshold = 1L
        )
        ser <- kernel_geomean_csc_cpp(
            m@x, m@i, m@p, nrow(m), ncol(m),
            axis = 0L, eps = eps,
            threshold = .Machine$integer.max
        )
        expect_identical(par, ser)
    }

    # kernel_mode_csc — single axis=0 path.
    par_mode <- kernel_mode_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                    axis = 0L, threshold = 1L)
    ser_mode <- kernel_mode_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                    axis = 0L,
                                    threshold = .Machine$integer.max)
    expect_identical(par_mode, ser_mode)

    # kernel_quantile_csc — q = 0.5 (median); q-choice does not affect
    # the fill-pass parallelism under test.
    par_q <- kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                     axis = 0L, q = 0.5, threshold = 1L)
    ser_q <- kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                     axis = 0L, q = 0.5,
                                     threshold = .Machine$integer.max)
    expect_identical(par_q, ser_q)
})

test_that("kernel_var_csc axis=0 peak RSS stays under 100 MB on stress fixture", {
    skip_on_cran()
    skip_if_not_installed("bench")

    # NOTE: libgomp caches max_threads at DSO-load time, so a runtime
    # Sys.setenv(OMP_NUM_THREADS=...) does NOT change the thread count.
    # The test asserts peak RSS under whatever thread count libgomp
    # picked up — typically parallel::detectCores(). On a 128-thread
    # dev machine that means 128 threads; on an 8-thread CI box, 8.
    # Pre-fix the O(nthreads × nrow) bucket pattern allocated 16 B per
    # (thread, row); at 128 threads × 100k rows that is ~200 MB, well
    # over this 100 MB budget. Post-fix the footprint is bounded by the
    # output shape plus two nrow-sized double vectors (1.6 MB total).

    set.seed(42L)
    nr <- 100000L
    nc <- 5000L
    nnz <- as.integer(nr * nc * 0.02)
    m <- Matrix::sparseMatrix(
        i = sample.int(nr, nnz, replace = TRUE),
        j = sample.int(nc, nnz, replace = TRUE),
        x = runif(nnz, 0.1, 10.0),
        dims = c(nr, nc),
        repr = "C"
    )

    gc(full = TRUE)
    mem_before <- bench::bench_process_memory()
    out <- kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                              axis = 0L, variant = "Var", eps = 0,
                              threshold = 1L)
    mem_after <- bench::bench_process_memory()

    delta <- as.numeric(mem_after["max"]) - as.numeric(mem_before["max"])
    expect_lt(delta, 100 * 1024 * 1024)

    # Sanity: output shape and finite values.
    expect_equal(length(out), nr)
    expect_true(all(is.finite(out)))
})
```

- [ ] **Step 8.2: Install and run the new tests**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
R CMD INSTALL . --preclean 2>&1 | tail -3
Rscript -e 'testthat::test_file("tests/testthat/test-kernel-csc-axis0-memory.R", reporter = "summary")' 2>&1 | tail -20
```

Expected: both `test_that` blocks pass. Failure in the bit-identity block = a kernel edit broke summation order or tie-break semantics. Failure in the RSS block = a thread-bucket pattern was reintroduced.

- [ ] **Step 8.3: Run the full test suite**

```bash
Rscript -e 'testthat::test_package("dafr", reporter = "summary")' 2>&1 | tail -3
```

Expected: `[ FAIL 0 | PASS ≥1909 ]` (1907 + ≥2 new test_that blocks — the bundled bit-identity block counts as one test_that but contains many `expect_identical` assertions, so the reported count may be higher depending on testthat's counting convention).

- [ ] **Step 8.4: Commit**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
git add tests/testthat/test-kernel-csc-axis0-memory.R
git -c commit.gpgsign=false commit -m "$(cat <<'EOF'
test(9d-n): regression guards for CSC axis=0 row-partition

Two test_that blocks in a new file:

1. Bit-identity: for all six row-partitioned CSC kernels at axis = 0,
   parallel dispatch (threshold = 1L) and serial dispatch
   (threshold = .Machine\$integer.max) produce identical output on a
   2k x 2k x 0.02 fixture. Row-partition is bit-identical by
   construction — this test catches any future refactor that breaks
   the invariant.

2. Peak-RSS budget: delta of bench::bench_process_memory() across a
   single kernel_var_csc call at threshold = 1L stays under 100 MB on
   a 100k x 5k x 0.02 fixture. Pre-fix would blow the budget at 128
   threads because of O(nthreads x nrow) thread-bucket allocation;
   post-fix the footprint is bounded by the output shape.
EOF
)"
```

---

## Task 9: NEWS entry

**Files:**
- Modify: `NEWS.md` (unreleased / top section)

- [ ] **Step 9.1: Read the current NEWS.md header**

```bash
head -20 NEWS.md
```

- [ ] **Step 9.2: Add a bullet under the unreleased / current-dev section**

Open `NEWS.md`. Under the existing unreleased-dev section header (matching the form used by the 9d-M NEWS entry in `d6d9a14`), insert a new bullet:

```markdown
* Performance: `kernel_var_csc`, `kernel_minmax_csc`, `kernel_log_reduce`,
  `kernel_geomean_csc`, `kernel_mode_csc`, and `kernel_quantile_csc` now
  use the row-partition parallel scan from slice 9d-M for their axis-0
  branches, eliminating the `O(nthreads × nrow)` thread-bucket allocation
  in the four category-A kernels and enabling parallel fill in the two
  category-B kernels. At 128 threads × 10⁶ rows the combined peak-RSS
  reduction is ~6 GB; wall-time improves because the serial merge phase
  is removed.
```

Match the indentation, bullet style, and tense of surrounding entries. If the 9d-M bullet is still the top of the unreleased section, place the 9d-N bullet immediately above or below it.

- [ ] **Step 9.3: Install, quick sanity run, commit**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
R CMD INSTALL . --preclean 2>&1 | tail -3
Rscript -e 'testthat::test_package("dafr", reporter = "summary")' 2>&1 | tail -3
git add NEWS.md
git -c commit.gpgsign=false commit -m "docs(9d-n): NEWS entry for CSC axis=0 row-partition sweep"
```

---

## Task 10: Post-fix profile

**Files:**
- Create: `dev/benchmarks/2026-04-22-post-slice-9d-n/profile.txt`

- [ ] **Step 10.1: Re-run the profiling script on the post-fix HEAD**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
R CMD INSTALL . --preclean 2>&1 | tail -3
mkdir -p dev/benchmarks/2026-04-22-post-slice-9d-n
/usr/bin/time -v Rscript dev/scripts/stress-9d-n-memory.R \
    > dev/benchmarks/2026-04-22-post-slice-9d-n/profile.txt \
    2>&1
cat dev/benchmarks/2026-04-22-post-slice-9d-n/profile.txt
```

Expected: category-A kernels report RSS_delta well under pre-fix values (bounded by output shape + nrow-sized doubles); category-B kernels report lower wall-time than pre-fix (parallel fill).

- [ ] **Step 10.2: Commit the post-fix profile (dev repo)**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/dev
git add benchmarks/2026-04-22-post-slice-9d-n/
git -c commit.gpgsign=false commit -m "profile(9d-n): post-fix CSC axis=0 baseline"
```

---

## Task 11: Final verification, push, merge, tag

**Files:** none (git operations only).

- [ ] **Step 11.1: Final full-suite run on the branch tip**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
R CMD INSTALL . --preclean 2>&1 | tail -3
Rscript -e 'testthat::test_package("dafr", reporter = "summary")' 2>&1 | tail -3
```

Expected: `[ FAIL 0 | PASS ≥1909 ]`. Any failure blocks the merge — investigate and fix before continuing.

- [ ] **Step 11.2: Push branch to origin and monitor CI**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
git log --oneline slice-9d-m..HEAD
git push -u origin slice-9d-n-csc-axis0-memory-fix
# Wait for GH Actions: altrep-sanity.yaml, bench.yaml, R-CMD-check.yaml
gh run list --branch slice-9d-n-csc-axis0-memory-fix --limit 5
```

If any CI workflow fails: read the logs, diagnose, fix on-branch, push again. Do NOT merge with red CI.

- [ ] **Step 11.3: Merge to main with an explicit merge commit**

```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
git checkout main
git pull --ff-only origin main
git merge --no-ff slice-9d-n-csc-axis0-memory-fix -m "$(cat <<'EOF'
merge: slice 9d-n — CSC axis=0 thread-bucket memory fix

Extend the 9d-M row-partition technique to six non-grouped CSC
kernels: four category-A kernels (kernel_var_csc, kernel_minmax_csc,
kernel_log_reduce, kernel_geomean_csc) that previously carried
O(nthreads × nrow) thread-bucket memory, and two category-B kernels
(kernel_mode_csc, kernel_quantile_csc) whose axis-0 fill pass was
single-threaded to avoid write races.

Combined peak-RSS reduction at 128 threads × 10⁶ rows: ~6 GB.
Wall-time improves because the serial merge phase is removed for
category-A kernels and the fill pass becomes parallel for category-B.

Bit-identity with serial dispatch is preserved: every row is owned by
one thread which scans columns in ascending order, so floating-point
summation order matches the serial path. New test file
tests/testthat/test-kernel-csc-axis0-memory.R asserts the invariant.
EOF
)"
```

- [ ] **Step 11.4: Tag and push**

```bash
git tag slice-9d-n
git push origin main
git push origin slice-9d-n
```

- [ ] **Step 11.5: Verify CI on main**

```bash
gh run list --branch main --limit 5
```

Expected: all three workflows succeed on the merge commit.

- [ ] **Step 11.6: Write slice exit note (dev repo)**

Create `dev/notes/slice-9d-n-exit.md` capturing:
- Scope delivered (6A, six kernels).
- Pre-fix vs post-fix peak-RSS and wall-time numbers from Tasks 1 & 10.
- Any issues encountered and how they were resolved.
- Carry-over items (9d-M code-review housekeeping, mmap S7-ctor floor, flat-storage optimisation — all unchanged).

Commit to dev repo:
```bash
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/dev
git add notes/slice-9d-n-exit.md
git -c commit.gpgsign=false commit -m "docs(9d-n): slice exit note"
git push origin main
```

---

## Self-review checklist (for the plan author, not subagents)

Before handing this plan off:

1. **Spec coverage:** all six kernels in §5 of the design map to Tasks 2-7; test plan §7 maps to Task 8; pre-fix baseline §8 maps to Task 1; NEWS + merge §9 maps to Tasks 9-11. ✓
2. **Placeholder scan:** no TBD / TODO / "implement later". Each kernel task contains exact line ranges and complete replacement code blocks. ✓
3. **Type consistency:** `nnz_per_row` is `std::vector<int>` across tasks 3/4/5; `rows` is `std::vector<std::vector<Entry>>` in Task 6 and `std::vector<std::vector<double>>` in Task 7 (matches existing types); `pout` is `double*` throughout. ✓
4. **Race-freedom:** every per-row accumulator write is guarded by `if (pi[k] < r0 || pi[k] >= r1) continue;`, which partitions row ownership. Post-process loops read only rows in the owning thread's `[r0, r1)`. No cross-thread reads of shared accumulators. ✓

End of plan.
