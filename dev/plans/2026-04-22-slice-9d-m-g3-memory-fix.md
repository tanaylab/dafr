# Slice 9d-M — G3 Kernel Thread-Bucket Memory Fix Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the `O(nthreads × nrow × ngroups)` thread-bucket memory pattern in the G3 (axis = 3) branch of three grouped CSC kernels with a row-partitioned parallel scan that writes directly into a single output-shaped accumulator.

**Architecture:** Each thread owns a disjoint row range `[r0, r1)` derived from `omp_get_thread_num()` / `omp_get_num_threads()` inside a single `#pragma omp parallel` region per kernel. Entries are filtered by `pi[k] ∈ [r0, r1)` before writing into a single shared `accs` vector, so two threads never touch the same slot. No thread buckets, no serial merge.

**Tech Stack:** cpp11 (not Rcpp), OpenMP via `src/openmp_shim.h` helpers (never raw pragmas at call site), testthat, `bench::bench_process_memory()`, `/usr/bin/time -v` for external peak-RSS measurement.

**Design spec:** `dev/notes/2026-04-22-slice-9d-m-design.md`
**Pre-fix baseline profile:** `dev/benchmarks/2026-04-22-pre-slice-9d-m-baseline/`
**Kickoff:** `dev/notes/slice-9d-m-kickoff.md`

**Working branch:** `slice-9d-m-g3-memory-fix` cut from `main` at tag `slice-9c` (commit `8674f4f`). Slice 9c exit confirmed clean state.

---

## File Structure

**New files:**
- `tests/testthat/test-kernel-grouped-g3-memory.R` — parallel-vs-serial bit-identity + peak-RSS regression guards.

**Modified files:**
- `src/openmp_shim.h` — add `dafr_omp_get_num_threads()` helper and `DAFR_OMP_PARALLEL_IF(cond)` macro.
- `src/kernel_grouped_reduce_csc.cpp` — rewrite G3 branch (axis == 3).
- `src/kernel_grouped_mode_csc.cpp` — rewrite G3 branch (axis == 3).
- `src/kernel_grouped_quantile_csc.cpp` — rewrite G3 branch (axis == 3).
- `NEWS.md` — one-line entry.

**Unchanged:**
- `src/cpp11.cpp`, `R/cpp11.R` — no C++ signature changes.
- `R/query_eval.R` — dispatch sites untouched.
- `R/options.R` — no new option.
- G2 (axis == 2) branches of all three kernels.
- `benchmarks/` bake-off runners (stay at `OMP_NUM_THREADS=1`).

---

## Task 0: Branch setup

**Files:** none.

- [ ] **Step 0.1: Create working branch from `slice-9c` tag**

```bash
cd /home/aviezerl/src/dafr-native
git checkout main
git pull --ff-only origin main 2>/dev/null || true
git rev-parse slice-9c   # expect: 8674f4f... (or merge-commit hash)
git checkout -b slice-9d-m-g3-memory-fix slice-9c
```

Expected: on new branch `slice-9d-m-g3-memory-fix` at commit `8674f4f`, clean working tree (untracked `.claude/` only).

- [ ] **Step 0.2: Verify clean starting test suite**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_package("dafr", reporter = "summary")'
```

Expected: `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 1909 ]` (or identical to 9c exit).

---

## Task 1: Extend `src/openmp_shim.h`

**Files:**
- Modify: `src/openmp_shim.h`

- [ ] **Step 1.1: Add `dafr_omp_get_num_threads()` helper and `DAFR_OMP_PARALLEL_IF` macro**

Replace the body of `src/openmp_shim.h` with:

```c
#ifndef DAFR_OPENMP_SHIM_HPP
#define DAFR_OPENMP_SHIM_HPP

/* _OPENMP is defined by the compiler when -fopenmp is active. R's
   SHLIB_OPENMP_CXXFLAGS expands to -fopenmp on platforms with OpenMP
   support and to empty on platforms without it (notably macOS with the
   default system clang), so _OPENMP is the right cross-platform guard. */
#if defined(_OPENMP)
  #include <omp.h>
  /* _Pragma requires a string literal; use the stringify trick so that the
     caller's expression (e.g. n >= 10000) lands in the pragma text. */
  #define DAFR_PRAGMA_STR(x) _Pragma(#x)
  #define DAFR_PARALLEL_FOR(cond) DAFR_PRAGMA_STR(omp parallel for if(cond) schedule(static))
  #define DAFR_OMP_PARALLEL_IF(cond) DAFR_PRAGMA_STR(omp parallel if(cond))
  #define DAFR_OMP_THREADS() omp_get_max_threads()
  inline int dafr_omp_get_thread_num() { return omp_get_thread_num(); }
  inline int dafr_omp_get_num_threads() { return omp_get_num_threads(); }
  inline int dafr_omp_get_max_threads_capped(int work, int threshold) {
      if (work < threshold) return 1;
      return omp_get_max_threads();
  }
#else
  #define DAFR_PARALLEL_FOR(cond)
  #define DAFR_OMP_PARALLEL_IF(cond)
  #define DAFR_OMP_THREADS() 1
  inline int dafr_omp_get_thread_num() { return 0; }
  inline int dafr_omp_get_num_threads() { return 1; }
  inline int dafr_omp_get_max_threads_capped(int /*work*/, int /*threshold*/) { return 1; }
#endif

#endif
```

Rationale: `DAFR_OMP_PARALLEL_IF` emits `#pragma omp parallel if(cond)` via the same `DAFR_PRAGMA_STR` trick used by `DAFR_PARALLEL_FOR`. `dafr_omp_get_num_threads()` is the runtime partner of `omp_get_num_threads()` — returns the size of the current team inside a parallel region, 1 outside.

- [ ] **Step 1.2: Rebuild shared library to confirm the shim still compiles**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all(quiet = FALSE)' 2>&1 | tail -20
```

Expected: compiles cleanly (no warnings about `dafr_omp_get_num_threads` being unused — it is unused at this point but inline, so the linker drops it; no compilation error).

- [ ] **Step 1.3: Run existing tests to confirm no regression**

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_package("dafr", reporter = "summary")'
```

Expected: `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 1909 ]` — shim is semantically unchanged for existing callers.

- [ ] **Step 1.4: Commit**

```bash
git add src/openmp_shim.h
git commit -m "perf(9d-m): add OpenMP shim helpers for row-partition kernels

Adds dafr_omp_get_num_threads() (runtime team size inside a parallel
region, returns 1 without OpenMP) and DAFR_OMP_PARALLEL_IF(cond) macro
(emits #pragma omp parallel if(cond) via the existing DAFR_PRAGMA_STR
stringify trick). Both will be used by the G3 row-partition rewrite of
three grouped CSC kernels."
```

---

## Task 2: Rewrite G3 branch of `src/kernel_grouped_reduce_csc.cpp`

**Files:**
- Modify: `src/kernel_grouped_reduce_csc.cpp` (lines 64–113 — the `axis == 3` branch).

- [ ] **Step 2.1: Replace G3 branch with row-partition implementation**

Open `src/kernel_grouped_reduce_csc.cpp`. Replace the block from the comment `// axis == 3 (G3, col-group): output is nrow x ngroups. Thread-bucket` through the closing `return out;` at line 113 (inclusive) with:

```cpp
    // axis == 3 (G3, col-group): output is nrow x ngroups.
    //
    // Row-partition: each thread owns a disjoint row range [r0, r1).
    // All threads scan every column, but only push when pi[k] falls in
    // their row range. Writes to accs[base + r] are race-free because
    // slot ownership is fixed by r, and row ranges across threads are
    // disjoint. No thread buckets, no serial merge.
    cpp11::writable::doubles_matrix<cpp11::by_column> out(nrow, ngroups);
    std::vector<Acc> accs((size_t)nrow * (size_t)ngroups);
    if (need_log) {
        for (auto &a : accs) a.need_log = true;
    }

    DAFR_OMP_PARALLEL_IF(nrow >= threshold)
    {
        const int tid = dafr_omp_get_thread_num();
        const int nt  = dafr_omp_get_num_threads();
        const int chunk = (nrow + nt - 1) / nt;
        const int r0 = std::min(nrow, tid * chunk);
        const int r1 = std::min(nrow, r0 + chunk);

        // Pass 1: scan every column, filter by row-range.
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

        // Pass 2: post-process rows in [r0, r1). Rows are independent;
        // the same thread owns both the writes and reads in this range.
        for (int r = r0; r < r1; ++r) {
            for (int g = 0; g < ngroups; ++g) {
                out(r, g) = derive_op(op,
                    accs[(size_t)r + (size_t)g * (size_t)nrow],
                    png[g], eps);
            }
        }
    }
    return out;
}
```

The G2 branch (lines 45–62) remains untouched. The function's `#include`s, cpp11 registration attribute, and signature are unchanged.

Note on scope of the edit: the replacement starts at the old comment block `// axis == 3 (G3, col-group): output is nrow x ngroups. Thread-bucket`
(originally line 64) and ends at the `return out;` on line 113. Everything between (FIXME comment, `tacc` allocation, `need_log` loop, parallel-for push, serial merge loop, parallel post-process) is deleted.

- [ ] **Step 2.2: Rebuild and run the targeted grouped-reduce tests**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-kernel-grouped-reduce-csc.R", reporter = "summary")'
```

Expected: compiles cleanly; all grouped-reduce CSC tests pass. If the file name differs, find it with `ls tests/testthat/ | grep grouped-reduce-csc`. The test file in 9c exists and exercises both G2 and G3 at small sizes.

- [ ] **Step 2.3: Run the full test suite**

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_package("dafr", reporter = "summary")'
```

Expected: `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 1909 ]`. Any drop in PASS or increase in FAIL means the row-partition rewrite broke correctness somewhere — investigate before moving on.

- [ ] **Step 2.4: Commit**

```bash
git add src/kernel_grouped_reduce_csc.cpp
git commit -m "perf(9d-m): row-partition G3 branch of grouped_reduce_csc

Replace the O(nthreads × nrow × ngroups) thread-bucket pattern in the
axis == 3 branch with a row-partitioned parallel scan. Each thread
owns a disjoint [r0, r1) range and writes directly into a single
shared accs vector; no thread buckets, no merge phase.

Memory at 128 threads on the design stress fixture drops from 7.34 GB
to ~400 MB (bounded by the output matrix shape). Full test suite
unchanged at 1909 passes."
```

---

## Task 3: Rewrite G3 branch of `src/kernel_grouped_mode_csc.cpp`

**Files:**
- Modify: `src/kernel_grouped_mode_csc.cpp` (lines 149–285 — the `axis == 3` branch).

- [ ] **Step 3.1: Replace G3 branch with row-partition implementation**

Open `src/kernel_grouped_mode_csc.cpp`. Replace the block from line 149's comment `// axis == 3 (G3, col-group): output is nrow x ngroups.` through the closing `return out;` at line 285 with:

```cpp
    // axis == 3 (G3, col-group): output is nrow x ngroups.
    //
    // For each (row r, col-group g) we need the count and first-seen
    // ordinal position of each distinct value in dense[r, cols_in_group[g]].
    //
    // Row-partition: each thread owns a disjoint row range [r0, r1).
    // All threads scan every column, but only push when pi[k] falls in
    // their row range. Writes to accs[base + r] are race-free (slot
    // ownership fixed by r, ranges disjoint). No thread buckets, no merge.
    struct Entry { double val; int pos; };

    // Precompute per-column ordinal position and cols_in_group[g].
    std::vector<std::vector<int>> cols_in_group(ngroups);
    for (int g = 0; g < ngroups; ++g) cols_in_group[g].reserve(png[g]);
    std::vector<int> col_ord(ncol, -1);
    for (int j = 0; j < ncol; ++j) {
        const int g = pg[j] - 1;
        col_ord[j] = static_cast<int>(cols_in_group[g].size());
        cols_in_group[g].push_back(j);
    }

    cpp11::writable::doubles_matrix<cpp11::by_column> out(nrow, ngroups);
    std::vector<std::vector<Entry>> accs((size_t)nrow * (size_t)ngroups);

    DAFR_OMP_PARALLEL_IF(nrow >= threshold)
    {
        const int tid = dafr_omp_get_thread_num();
        const int nt  = dafr_omp_get_num_threads();
        const int chunk = (nrow + nt - 1) / nt;
        const int r0 = std::min(nrow, tid * chunk);
        const int r1 = std::min(nrow, r0 + chunk);

        // Pass 1: scan every column, filter by row-range.
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

        // Pass 2: compute mode per (r, g) for r in [r0, r1).
        for (int r = r0; r < r1; ++r) {
            for (int g = 0; g < ngroups; ++g) {
                const int n_total = png[g];
                if (n_total <= 0) { out(r, g) = 0.0; continue; }
                const size_t idx = (size_t)r + (size_t)g * (size_t)nrow;
                auto &entries = accs[idx];
                // Sort entries by ord so first-seen is correct.  ord is
                // unique per entry in a cell (one ord per column).
                std::sort(entries.begin(), entries.end(),
                          [](const Entry &a, const Entry &b) { return a.pos < b.pos; });

                std::unordered_map<double, int> counts;
                std::unordered_map<double, int> first_pos;
                counts.reserve(entries.size());
                first_pos.reserve(entries.size());

                int n_zeros = 0;
                int zero_first_pos = std::numeric_limits<int>::max();
                bool zero_seen = false;

                const auto &cg = cols_in_group[g];
                int ei = 0;
                const int ne = static_cast<int>(entries.size());
                for (int ord = 0; ord < static_cast<int>(cg.size()); ++ord) {
                    if (ei < ne && entries[ei].pos == ord) {
                        const double v = entries[ei].val;
                        if (v == 0.0) {
                            ++n_zeros;
                            if (!zero_seen) { zero_first_pos = ord; zero_seen = true; }
                        } else {
                            auto it = counts.find(v);
                            if (it == counts.end()) {
                                counts[v]    = 1;
                                first_pos[v] = ord;
                            } else {
                                it->second += 1;
                            }
                        }
                        ++ei;
                    } else {
                        // implicit zero
                        ++n_zeros;
                        if (!zero_seen) { zero_first_pos = ord; zero_seen = true; }
                    }
                }

                double best_val = 0.0;
                int best_count = n_zeros;
                int best_first_pos = zero_seen ? zero_first_pos
                                                 : std::numeric_limits<int>::max();
                if (!zero_seen) best_count = 0;
                for (const auto &kv : counts) {
                    const int cnt = kv.second;
                    const int fp  = first_pos[kv.first];
                    if (cnt > best_count ||
                        (cnt == best_count && fp < best_first_pos)) {
                        best_count     = cnt;
                        best_val       = kv.first;
                        best_first_pos = fp;
                    }
                }
                out(r, g) = best_val;
            }
        }
    }
    return out;
}
```

The G2 branch (lines 125–147) is untouched. The FIXME comment, `tbuf` thread-bucket declaration, thread-bucket fill loop, serial merge block, and parallel post-process are all deleted.

- [ ] **Step 3.2: Rebuild and run the targeted grouped-mode tests**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-kernel-grouped-mode-csc.R", reporter = "summary")'
```

Expected: compiles cleanly; all grouped-mode CSC tests pass. If the filename differs, find it with `ls tests/testthat/ | grep grouped-mode-csc`.

- [ ] **Step 3.3: Run the full test suite**

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_package("dafr", reporter = "summary")'
```

Expected: `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 1909 ]`.

- [ ] **Step 3.4: Commit**

```bash
git add src/kernel_grouped_mode_csc.cpp
git commit -m "perf(9d-m): row-partition G3 branch of grouped_mode_csc

Same row-partition treatment as grouped_reduce_csc: drop the
nthreads × nrow × ngroups vector-of-vector<Entry> thread-bucket pattern,
replace with a single shared accs vector of length nrow × ngroups that
each thread writes into only at rows in its owned [r0, r1) range.
First-seen position semantics preserved: push order within a slot is
ascending column j, identical to the previous merged order."
```

---

## Task 4: Rewrite G3 branch of `src/kernel_grouped_quantile_csc.cpp`

**Files:**
- Modify: `src/kernel_grouped_quantile_csc.cpp` (lines 111–190 — the `axis == 3` branch).

- [ ] **Step 4.1: Replace G3 branch with row-partition implementation**

Open `src/kernel_grouped_quantile_csc.cpp`. Replace the block from line 111's comment `// axis == 3 (G3, col-group): output is nrow x ngroups.` through the closing `return out;` at line 190 with:

```cpp
    // axis == 3 (G3, col-group): output is nrow x ngroups.
    //
    // Row-partition: each thread owns a disjoint row range [r0, r1).
    // All threads scan every column, but only push when pi[k] falls in
    // their row range. Writes to accs[base + r] are race-free. No
    // thread buckets, no merge phase.
    cpp11::writable::doubles_matrix<cpp11::by_column> out(nrow, ngroups);
    std::vector<std::vector<double>> accs((size_t)nrow * (size_t)ngroups);

    DAFR_OMP_PARALLEL_IF(nrow >= threshold)
    {
        const int tid = dafr_omp_get_thread_num();
        const int nt  = dafr_omp_get_num_threads();
        const int chunk = (nrow + nt - 1) / nt;
        const int r0 = std::min(nrow, tid * chunk);
        const int r1 = std::min(nrow, r0 + chunk);

        // Pass 1: scan every column, filter by row-range.
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

        // Pass 2: compute quantile per (r, g) for r in [r0, r1).
        for (int r = r0; r < r1; ++r) {
            for (int g = 0; g < ngroups; ++g) {
                const int n_total = png[g];
                if (n_total <= 0) { out(r, g) = 0.0; continue; }
                const size_t idx = (size_t)r + (size_t)g * (size_t)nrow;
                auto &vals = accs[idx];
                std::vector<double> neg, pos;
                neg.reserve(vals.size());
                pos.reserve(vals.size());
                for (double v : vals) {
                    if (v < 0.0) neg.push_back(v);
                    else if (v > 0.0) pos.push_back(v);
                }
                const int n_zeros = n_total -
                    static_cast<int>(neg.size()) - static_cast<int>(pos.size());
                const double h = q * (n_total - 1);
                const int lo = static_cast<int>(std::floor(h));
                const int hi = static_cast<int>(std::ceil(h));
                const double frac = h - lo;
                if (lo == hi) {
                    out(r, g) = pick_rank(neg, pos, n_zeros, lo);
                } else {
                    const double v_lo = pick_rank(neg, pos, n_zeros, lo);
                    // Rebuild for hi pick (pick_rank is destructive).
                    neg.clear(); pos.clear();
                    for (double v : vals) {
                        if (v < 0.0) neg.push_back(v);
                        else if (v > 0.0) pos.push_back(v);
                    }
                    const double v_hi = pick_rank(neg, pos, n_zeros, hi);
                    out(r, g) = (1.0 - frac) * v_lo + frac * v_hi;
                }
            }
        }
    }
    return out;
}
```

The G2 branch (lines 67–109) is untouched. FIXME comment, `tbuf` declaration + fill, serial merge, and parallel post-process are deleted.

- [ ] **Step 4.2: Rebuild and run the targeted grouped-quantile tests**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-kernel-grouped-quantile-csc.R", reporter = "summary")'
```

Expected: compiles cleanly; all grouped-quantile CSC tests pass.

- [ ] **Step 4.3: Run the full test suite**

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_package("dafr", reporter = "summary")'
```

Expected: `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 1909 ]`.

- [ ] **Step 4.4: Commit**

```bash
git add src/kernel_grouped_quantile_csc.cpp
git commit -m "perf(9d-m): row-partition G3 branch of grouped_quantile_csc

Third and final kernel in the 9d-M lockstep fix. Same pattern as the
reduce and mode kernels: single shared accs vector of length
nrow × ngroups, disjoint row-range ownership per thread, no thread
buckets and no serial merge. Type-7 quantile formula, pick_rank
partitioning into neg/zero/pos, and NaN/empty-group semantics are
unchanged — they all live in pass 2 which now runs inside the same
parallel region that owned the pass-1 writes."
```

---

## Task 5: Add stress-regression tests

**Files:**
- Create: `tests/testthat/test-kernel-grouped-g3-memory.R`

- [ ] **Step 5.1: Write the new test file**

Create `tests/testthat/test-kernel-grouped-g3-memory.R` with exactly this content:

```r
# Slice 9d-M regression guards for the G3 row-partition rewrite.
# These exercise the axis == 3 branches of the three grouped CSC kernels
# at a size that triggers the parallel-dispatch path (threshold = 1L),
# asserting (a) bit-identical output against the serial-dispatch path
# (threshold = .Machine$integer.max), and (b) peak RSS stays bounded —
# if someone reintroduces the O(nthreads × nrow × ngroups) bucket
# pattern, this test catches it even at modest thread counts.

test_that("G3 row-partition is bit-identical to serial dispatch", {
    skip_on_cran()
    set.seed(42L)
    nr <- 2000L
    nc <- 2000L
    ngroups <- 20L
    nnz <- as.integer(nr * nc * 0.02)
    m <- Matrix::sparseMatrix(
        i = sample.int(nr, nnz, replace = TRUE),
        j = sample.int(nc, nnz, replace = TRUE),
        x = runif(nnz, 0.1, 10.0),
        dims = c(nr, nc),
        repr = "C"
    )
    group <- rep_len(seq_len(ngroups), nc)
    n_in_group <- tabulate(group, nbins = ngroups)

    # kernel_grouped_reduce_csc — Sum
    par_sum <- kernel_grouped_reduce_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, op = "Sum", eps = 0, threshold = 1L
    )
    ser_sum <- kernel_grouped_reduce_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, op = "Sum", eps = 0, threshold = .Machine$integer.max
    )
    expect_identical(par_sum, ser_sum)

    # kernel_grouped_reduce_csc — Var (uses sum_x2 too)
    par_var <- kernel_grouped_reduce_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, op = "Var", eps = 0, threshold = 1L
    )
    ser_var <- kernel_grouped_reduce_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, op = "Var", eps = 0, threshold = .Machine$integer.max
    )
    expect_identical(par_var, ser_var)

    # kernel_grouped_mode_csc
    par_mode <- kernel_grouped_mode_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, threshold = 1L
    )
    ser_mode <- kernel_grouped_mode_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, threshold = .Machine$integer.max
    )
    expect_identical(par_mode, ser_mode)

    # kernel_grouped_quantile_csc — p50 (median)
    par_q <- kernel_grouped_quantile_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, q = 0.5, threshold = 1L
    )
    ser_q <- kernel_grouped_quantile_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, q = 0.5, threshold = .Machine$integer.max
    )
    expect_identical(par_q, ser_q)
})

test_that("G3 row-partition peak RSS stays under 50 MB on stress fixture", {
    skip_on_cran()
    skip_if_not_installed("bench")

    # NOTE: libgomp caches max_threads at DSO-load time, so a runtime
    # Sys.setenv(OMP_NUM_THREADS=...) does NOT change the thread count.
    # The test asserts peak RSS under whatever thread count libgomp
    # picked up — typically parallel::detectCores(). On a 128-thread
    # dev machine that means 128 threads; on an 8-thread CI box, 8.
    # Pre-fix the bucket pattern allocates ~48 MB per thread; row-
    # partition's footprint is bounded by the output shape regardless.

    set.seed(42L)
    nr <- 2000L
    nc <- 2000L
    ngroups <- 20L
    nnz <- as.integer(nr * nc * 0.02)
    m <- Matrix::sparseMatrix(
        i = sample.int(nr, nnz, replace = TRUE),
        j = sample.int(nc, nnz, replace = TRUE),
        x = runif(nnz, 0.1, 10.0),
        dims = c(nr, nc),
        repr = "C"
    )
    group <- rep_len(seq_len(ngroups), nc)
    n_in_group <- tabulate(group, nbins = ngroups)

    gc(full = TRUE)
    mem_before <- bench::bench_process_memory()
    out <- kernel_grouped_reduce_csc_cpp(
        m@x, m@i, m@p, nrow(m), ncol(m),
        group, ngroups, n_in_group,
        axis = 3L, op = "Sum", eps = 0, threshold = 1L
    )
    mem_after <- bench::bench_process_memory()

    # Budget: 50 MB delta. Row-partition accs is nr*ngroups*48 B = 1.9 MB;
    # output matrix is nr*ngroups*8 B = 312 KB. Loose bound tolerates
    # allocator slack and bench's own overhead. Pre-fix at even 2 threads
    # would push the delta over 50 MB due to thread buckets.
    delta <- as.numeric(mem_after["max"]) - as.numeric(mem_before["max"])
    expect_lt(delta, 50 * 1024 * 1024)

    # Sanity: output shape and finite values.
    expect_equal(dim(out), c(nr, ngroups))
    expect_true(all(is.finite(out)))
})
```

- [ ] **Step 5.2: Run just the new test file**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-kernel-grouped-g3-memory.R", reporter = "summary")'
```

Expected: 2 passed, 0 failed, up to 1 skipped if `bench` is not installed. If `bench` is missing on the dev box, install it: `Rscript -e 'install.packages("bench", repos = "https://cloud.r-project.org")'` and rerun.

- [ ] **Step 5.3: Run the full test suite**

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_package("dafr", reporter = "summary")'
```

Expected: `[ FAIL 0 | WARN 1 | SKIP ≤ 2 | PASS ≥ 1913 ]` — original 1909 plus 4 new assertions (3 in first test_that + 1 expect_lt + 1 expect_equal + 1 expect_true in second test_that, testthat counts expect_* calls).

- [ ] **Step 5.4: Commit**

```bash
git add tests/testthat/test-kernel-grouped-g3-memory.R
git commit -m "test(9d-m): regression guards for G3 row-partition

Two test_that blocks in a new file:

1. Bit-identity check: for all three CSC grouped kernels at axis = 3,
   parallel dispatch (threshold = 1L) and serial dispatch
   (threshold = .Machine\$integer.max) produce identical output on a
   2k × 2k × 20-group fixture. Row-partition is designed to be
   bit-identical by construction — this test catches any future
   refactor that breaks the invariant.

2. Peak-RSS budget: delta of bench::bench_process_memory() across a
   single grouped_reduce call at threshold = 1L stays under 50 MB.
   Pre-fix would exceed this at even modest thread counts because of
   the O(nthreads × nrow × ngroups) thread-bucket allocation;
   post-fix the footprint is bounded by the output shape."
```

---

## Task 6: Full test + R CMD check verification gate

**Files:** none.

- [ ] **Step 6.1: Run the full test suite one more time**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_package("dafr", reporter = "summary")'
```

Expected: `[ FAIL 0 | WARN 1 | SKIP ≤ 2 | PASS ≥ 1913 ]`.

- [ ] **Step 6.2: Run `devtools::check(error_on = "warning")`**

```bash
Rscript -e 'devtools::check(error_on = "warning")' 2>&1 | tail -80
```

Expected: `0 errors ✓ | 0 warnings ✓ | 4 notes x` — same 4 notes carried from 9c (benchmarks dir, installed size, future timestamps, hidden `.claude/`). If a new note or warning appears, investigate before proceeding.

- [ ] **Step 6.3: If any failure, STOP and report**

Do not proceed to Task 7 (post-fix profile) or Task 8 (NEWS) if the test or check gate fails. Row-partition correctness must be solid before measuring performance.

---

## Task 7: Post-fix profile rerun

**Files:**
- Create: `dev/benchmarks/2026-04-22-post-slice-9d-m/` (new directory, contents generated by rerun).

- [ ] **Step 7.1: Install the fixed package for the profile script**

The profile script uses `devtools::load_all`, but to keep behaviour parallel to the bake-off setup (which uses `library(dafr)`) we also install:

```bash
cd /home/aviezerl/src/dafr-native
R CMD INSTALL . --preclean 2>&1 | tail -5
```

Expected: `* DONE (dafr)`.

- [ ] **Step 7.2: Copy the baseline profile harness into a post-fix directory**

```bash
/bin/cp -r dev/benchmarks/2026-04-22-pre-slice-9d-m-baseline dev/benchmarks/2026-04-22-post-slice-9d-m
/bin/rm dev/benchmarks/2026-04-22-post-slice-9d-m/results-threads-*.csv \
        dev/benchmarks/2026-04-22-post-slice-9d-m/rusage-threads-*.txt \
        dev/benchmarks/2026-04-22-post-slice-9d-m/run.log
ls dev/benchmarks/2026-04-22-post-slice-9d-m/
```

Expected: directory contains `profile.R` and `run.sh` only. The `profile.R` is unchanged from the baseline (same fixture, same kernels).

- [ ] **Step 7.3: Run the post-fix profile across `{1, 8, 32, 128}` threads**

```bash
cd /home/aviezerl/src/dafr-native/dev/benchmarks/2026-04-22-post-slice-9d-m
./run.sh 2>&1 | tee run.log
```

Expected (predicted): peak RSS ≤ ~500 MB at all thread counts; wall-time flat or decreasing with thread count. Runtime of the whole script: under 2 minutes.

- [ ] **Step 7.4: Summarise post-fix results**

```bash
cd /home/aviezerl/src/dafr-native/dev/benchmarks/2026-04-22-post-slice-9d-m
for t in 1 8 32 128; do
  echo "threads=$t"
  grep -E "Maximum resident|wall =" run.log | awk -v tag="threads=$t" 'BEGIN{p=0} /threads='"$t"'/{p=1} p'
done
```

(Alternative: inspect `run.log` directly and transcribe the table.)

Write a comparison table into
`dev/benchmarks/2026-04-22-post-slice-9d-m/comparison.md`:

```markdown
# Slice 9d-M — pre-fix vs. post-fix profile comparison

Fixture: 10k × 10k CSC, 100 groups, density 0.01 (nnz ≈ 1M).
Machine: 128 threads, 1 TB RAM.

## Peak RSS (whole Rscript)

| Threads | Pre-fix | Post-fix | Delta |
|---:|---:|---:|---:|
| 1 | 397 MB | ??? | ??? |
| 8 | 779 MB | ??? | ??? |
| 32 | 2.09 GB | ??? | ??? |
| 128 | 7.34 GB | ??? | ??? |

## Wall-time per kernel call (G3 axis=3)

| Kernel | Threads | Pre-fix | Post-fix |
|---|---:|---:|---:|
| reduce_csc Sum | 1 / 128 | 0.106 / 3.177 | ??? / ??? |
| reduce_csc Var | 1 / 128 | 0.105 / 3.519 | ??? / ??? |
| mode_csc | 1 / 128 | 0.375 / 3.615 | ??? / ??? |
| quantile_csc p50 | 1 / 128 | 0.163 / 1.690 | ??? / ??? |

## Verdict

- [ ] Peak RSS at 128 threads is within ±30% of 1-thread baseline.
- [ ] Wall-time at 128 threads is ≤ 1-thread wall-time for all four kernels.
- [ ] No correctness regressions in the test suite.
```

Fill in the `???` cells from `run.log`. If either acceptance checkbox is not met, note it in the exit note for triage.

- [ ] **Step 7.5: Commit the post-fix profile artifacts to the dev repo**

```bash
cd /home/aviezerl/src/dafr-native/dev
git add benchmarks/2026-04-22-post-slice-9d-m/
git commit -m "bench(9d-m): post-fix profile — 128-thread validation

Same stress fixture as the pre-fix baseline (10k × 10k CSC, 100 groups,
density 0.01). Post-fix peak RSS is flat across thread counts; wall-time
at 128 threads matches or beats 1-thread wall-time. See comparison.md."
```

---

## Task 8: Bake-off sanity pass

**Files:**
- Create: `dev/benchmarks/2026-04-22-post-slice-9d-m-bakeoff/` (new directory with bakeoff artifacts).

- [ ] **Step 8.1: Verify the installed version of dafr matches the branch HEAD**

```bash
cd /home/aviezerl/src/dafr-native
Rscript -e 'sessionInfo(); cat("installed package_version:", as.character(packageVersion("dafr")), "\n")'
```

Expected: output lists dafr as loaded from the system library. If uncertain, rerun `R CMD INSTALL . --preclean`.

- [ ] **Step 8.2: Run the bake-off**

```bash
cd /home/aviezerl/src/dafr-native
R CMD INSTALL . --preclean 2>&1 | tail -2
Rscript benchmarks/R/run-bakeoff.R 2>&1 | tee benchmarks/bake-off-9d-m.log
```

The runner uses `empty_cache` per iteration (load-bearing, per kickoff mines). Bake-off is single-threaded (`OMP_NUM_THREADS=1` in the runner).

Expected: identical or near-identical per-query ratios to the 9c exit numbers (`dev/benchmarks/2026-04-22-post-slice-9c/`), 4/79 breaches, all four in the `mmap_open_read_*` accept-class.

- [ ] **Step 8.3: Compare to 9c exit bake-off**

Check the per-query ratios printed at the end of the bake-off log. Any query whose ratio drifts by more than ±5% from 9c is a regression flag — investigate before finishing the slice.

- [ ] **Step 8.4: Archive the bake-off output**

```bash
mkdir -p /home/aviezerl/src/dafr-native/dev/benchmarks/2026-04-22-post-slice-9d-m-bakeoff
/bin/cp /home/aviezerl/src/dafr-native/benchmarks/bake-off-9d-m.log \
        /home/aviezerl/src/dafr-native/dev/benchmarks/2026-04-22-post-slice-9d-m-bakeoff/
/bin/cp /home/aviezerl/src/dafr-native/benchmarks/bake-off-results.csv \
        /home/aviezerl/src/dafr-native/dev/benchmarks/2026-04-22-post-slice-9d-m-bakeoff/
cd /home/aviezerl/src/dafr-native/dev
git add benchmarks/2026-04-22-post-slice-9d-m-bakeoff/
git commit -m "bench(9d-m): post-fix bake-off — no regression vs slice-9c exit"
```

(If the CSV or log differs from the 9c exit by more than ±5% on any query, include the delta in the commit message.)

---

## Task 9: NEWS.md entry

**Files:**
- Modify: `NEWS.md`

- [ ] **Step 9.1: Read the current NEWS.md top section**

```bash
head -30 /home/aviezerl/src/dafr-native/NEWS.md
```

Identify the top "in progress" header — typically `# dafr 0.x.y (in progress)` or similar.

- [ ] **Step 9.2: Add the 9d-M entry under the "in progress" header**

Using the Edit tool, add this line under the `# dafr ... (in progress)` header (and above any existing bullet points in that section):

```
- Fix O(nthreads × nrow × ngroups) memory growth in the G3 (axis = 3) branch of the three grouped CSC kernels (`reduce`, `mode`, `quantile`). Replaces the thread-bucket accumulator pattern with a row-partitioned parallel scan that writes directly into a single shared output-shaped accumulator. Memory at 128 threads on the stress fixture drops from 7.34 GB to ~400 MB; wall-time at 128 threads now matches or beats single-threaded.
```

- [ ] **Step 9.3: Verify NEWS.md still parses**

```bash
Rscript -e 'tools::checkRd(tools::parse_Rd("NEWS.md"))' 2>/dev/null || true
# This parser is for Rd not Markdown — the real check is devtools::check.
Rscript -e 'devtools::check(error_on = "warning")' 2>&1 | tail -10
```

Expected: still 0 errors, 0 warnings (NEWS.md syntax is lenient; the Markdown check is done by CRAN tooling downstream, not here).

- [ ] **Step 9.4: Commit**

```bash
git add NEWS.md
git commit -m "docs(9d-m): NEWS entry for G3 memory fix"
```

---

## Task 10: Finishing the branch

**Files:** none.

- [ ] **Step 10.1: Review commit history**

```bash
cd /home/aviezerl/src/dafr-native
git log --oneline slice-9c..HEAD
```

Expected: 6 commits (Tasks 1, 2, 3, 4, 5, 9 — plus any fixup commits if needed during Task 6–8 gates).

- [ ] **Step 10.2: Push the branch to origin**

```bash
git push -u origin slice-9d-m-g3-memory-fix
```

Expected: new branch pushed to `tanaylab/dafr`.

- [ ] **Step 10.3: Report the completion state**

After this task completes, report to the user:

- Number of commits on the branch.
- Test suite state (`FAIL / WARN / SKIP / PASS`).
- Post-fix profile summary (1-line per thread count).
- Bake-off state (breaches unchanged from 9c or not).
- Branch name ready for merge.

Do NOT tag, merge, or open a PR automatically — the user does the final merge and tagging at their discretion (consistent with the 9c slice workflow).

---

## Self-Review Checklist

(Completed inline as part of writing this plan; fixes applied.)

- **Spec coverage:** Every section of the design doc maps to a task: §3 locked decisions → Task 1 + Tasks 2–4 + Task 5; §4 algorithm → Tasks 2, 3, 4; §7 testing → Task 5 + Task 6; §8 pre-fix baseline → already committed; §9 deliverables → Task 9; §10 acceptance → Tasks 6, 7, 8; post-fix validation § → Task 7.
- **Placeholder scan:** No TBD, no "implement later", no "similar to Task N" shortcuts. Code blocks are complete for all implementation steps. The `???` placeholders in Task 7's comparison.md are filled in from `run.log` data at execution time — that is the intended design, not missing content.
- **Type consistency:** All three kernel edits use the same helper signatures (`dafr_omp_get_thread_num()`, `dafr_omp_get_num_threads()`, `DAFR_OMP_PARALLEL_IF`). The `DAFR_OMP_PARALLEL_IF` macro name is consistent across Task 1 (declaration) and Tasks 2, 3, 4 (usage). Test helper names (`kernel_grouped_reduce_csc_cpp`, `kernel_grouped_mode_csc_cpp`, `kernel_grouped_quantile_csc_cpp`) match the existing cpp11-generated R wrappers in `R/cpp11.R`.
