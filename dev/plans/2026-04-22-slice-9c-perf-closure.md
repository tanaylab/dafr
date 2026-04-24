# Slice 9c — Perf Closure Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the 4 remaining dense-path bake-off breaches (`julia_queries_026` Quantile, `_028` Mode, `_043` G2 Max, `_047` G3 Max) by adding three new Int-aware C++ kernels and wiring them into `R/query_eval.R`.

**Architecture:** Three sibling cpp11 kernels mirror the existing CSC-layout kernels (`kernel_quantile_csc.cpp`, `kernel_mode_csc.cpp`, `kernel_minmax_csc.cpp`). Each accepts `SEXP mat` and branches on `TYPEOF(mat)` to handle INTSXP or REALSXP without an up-front `storage.mode(m) <- "double"` copy — this eliminates the ~2 ms ALTREP-materialize+allocate+copy tax that dominates light-tier queries on the `cells_daf` UMIs fixture. Dispatch in `R/query_eval.R` replaces four `matrixStats::*` / `apply(..., op_mode)` branches with calls to the new kernels. A post-process helper `.minmax_empty_to_na` converts `±Inf` empty-group sentinels to `NA_REAL`.

**Tech Stack:** cpp11 (NOT Rcpp), OpenMP via `openmp_shim.h`, cpp11 auto-registration, testthat, matrixStats (reference-only), R S7 for query dispatch.

**Source of truth — read before starting:**
- Design spec: `dev/notes/2026-04-22-slice-9c-design.md`
- Kickoff: `dev/notes/slice-9c-kickoff.md`
- Template kernels: `src/kernel_quantile_csc.cpp`, `src/kernel_mode_csc.cpp`, `src/kernel_grouped_rowsum_dense.cpp`
- Formula authority: `R/operations.R` `.op_quantile`, `.op_mode` (must bit-match)
- OpenMP shim: `src/openmp_shim.h`

---

## File Structure

**New C++ kernels (package repo):**
| File | Responsibility |
|---|---|
| `src/kernel_quantile_dense.cpp` | Per-row or per-column type-7 quantile on dense INTSXP/REALSXP. Entry: `kernel_quantile_dense_cpp(mat, axis, q, threshold)`. |
| `src/kernel_mode_dense.cpp` | Per-row or per-column mode (first-seen tiebreak) on dense INTSXP/REALSXP. Entry: `kernel_mode_dense_cpp(mat, axis, threshold)`. |
| `src/kernel_grouped_minmax_dense.cpp` | G2/G3 grouped Min or Max on dense INTSXP/REALSXP. Entry: `kernel_grouped_minmax_dense_cpp(mat, groups, ngroups, axis, variant)`. |

**New test files:**
| File | Responsibility |
|---|---|
| `tests/testthat/test-kernel-dense-quantile.R` | Unit tests for the dense Quantile kernel. |
| `tests/testthat/test-kernel-dense-mode.R` | Unit tests for the dense Mode kernel. |
| `tests/testthat/test-kernel-grouped-minmax-dense.R` | Unit tests for the grouped Min/Max kernel. |

**Regenerated (cpp11 auto-generated):**
- `src/cpp11.cpp`
- `R/cpp11.R`

**Edited (dispatch layer):**
- `R/query_eval.R` — four dispatch sites + one helper.
- `NEWS.md` — one bullet.

**Unchanged (confirm no edit):**
- `R/operations.R` (formula authority)
- `DESCRIPTION`, `NAMESPACE`, `Makevars`, `Makevars.win`
- All other `.cpp` and test files

## Task Dependency Graph

```
                           (parallel group A)
Task 1 ──┐
Task 2 ──┤                 (3 kernels — independent worktrees)
Task 3 ──┘
          └──> Task 4 (merge + regen + kernel-only validation)
                  └──> Task 5 (R dispatch wiring; 4 sites in one file)
                          └──> Task 6 (bake-off re-run + perf-log)
                                  └──> Task 7 (NEWS + slice exit)
```

Tasks 1, 2, 3 are independent and should be dispatched to parallel subagents (Opus or Sonnet — mechanical C++ work). Tasks 4–7 are sequential; each depends on the previous.

---

### Task 1: Dense Quantile kernel

**Files:**
- Create: `src/kernel_quantile_dense.cpp`
- Create: `tests/testthat/test-kernel-dense-quantile.R`

This task writes the kernel and the unit tests. Each subagent runs `cpp11::cpp_register()` + `devtools::load_all()` in its worktree to validate. The cpp11 regen files (`src/cpp11.cpp`, `R/cpp11.R`) will conflict at merge; Task 4 resolves by re-running regen on the merged source.

- [ ] **Step 1.1: Read the template kernel**

Read `src/kernel_quantile_csc.cpp` (165 lines) to internalize the type-7 formula and partition pattern. Note that the dense case does NOT need the neg/zero/pos split (no implicit zeros in dense).

- [ ] **Step 1.2: Write the kernel source file**

Create `src/kernel_quantile_dense.cpp`:

```cpp
// src/kernel_quantile_dense.cpp
// Per-column or per-row type-7 quantile on a dense numeric matrix.
// Accepts INTSXP or REALSXP without an up-front storage-mode copy.
//
// Matches stats::quantile(x, q, type = 7):
//   h    = q * (n - 1)
//   lo   = floor(h); hi = ceil(h); frac = h - lo
//   out  = (1 - frac) * v[lo] + frac * v[hi]
//
// NA / NaN semantics (per design spec §6):
//   - Any NA_INTEGER, NaN, or NA_REAL in the column/row -> output is NA_REAL
//     for that column/row.  Short-circuited at value-scan time.
//   - Empty input (n == 0) -> NA_REAL.
//
// axis == 1: per-column (ReduceToRow direction), output length = ncol
// axis == 0: per-row    (ReduceToColumn direction), output length = nrow

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <algorithm>
#include <cmath>
#include <vector>

namespace {

// Compute type-7 quantile on a complete scratch buffer in-place via nth_element.
// Returns NA_REAL if the buffer is empty.  Caller must filter NA/NaN upstream.
double quantile_inplace(std::vector<double>& buf, double q) {
    const int n = static_cast<int>(buf.size());
    if (n == 0) return NA_REAL;
    if (n == 1) return buf[0];
    const double h = q * (n - 1);
    const int lo = static_cast<int>(std::floor(h));
    const int hi = static_cast<int>(std::ceil(h));
    const double frac = h - lo;
    std::nth_element(buf.begin(), buf.begin() + lo, buf.end());
    const double v_lo = buf[lo];
    if (lo == hi) return v_lo;
    // buf is now partitioned around lo; everything >= v_lo is in [lo, end).
    // The hi-th element is the min of that tail -> nth_element(begin+lo+1, begin+hi, end)
    // on the tail.  Simplest: nth_element again from scratch on the full buffer.
    std::nth_element(buf.begin(), buf.begin() + hi, buf.end());
    const double v_hi = buf[hi];
    return (1.0 - frac) * v_lo + frac * v_hi;
}

// Fill scratch buffer from column j (axis=1) of dense matrix.  Returns false
// (and leaves buf unspecified) if any NA/NaN is encountered.
inline bool fill_col_int(const int* xint, int col_offset, int nrow,
                         std::vector<double>& buf) {
    buf.clear();
    buf.reserve(nrow);
    for (int i = 0; i < nrow; ++i) {
        const int vi = xint[col_offset + i];
        if (vi == NA_INTEGER) return false;
        buf.push_back(static_cast<double>(vi));
    }
    return true;
}

inline bool fill_col_dbl(const double* xdbl, int col_offset, int nrow,
                         std::vector<double>& buf) {
    buf.clear();
    buf.reserve(nrow);
    for (int i = 0; i < nrow; ++i) {
        const double v = xdbl[col_offset + i];
        if (ISNAN(v)) return false;     // covers both NA_REAL and NaN
        buf.push_back(v);
    }
    return true;
}

inline bool fill_row_int(const int* xint, int row, int nrow, int ncol,
                         std::vector<double>& buf) {
    buf.clear();
    buf.reserve(ncol);
    for (int j = 0; j < ncol; ++j) {
        const int vi = xint[j * nrow + row];
        if (vi == NA_INTEGER) return false;
        buf.push_back(static_cast<double>(vi));
    }
    return true;
}

inline bool fill_row_dbl(const double* xdbl, int row, int nrow, int ncol,
                         std::vector<double>& buf) {
    buf.clear();
    buf.reserve(ncol);
    for (int j = 0; j < ncol; ++j) {
        const double v = xdbl[j * nrow + row];
        if (ISNAN(v)) return false;
        buf.push_back(v);
    }
    return true;
}

} // namespace

[[cpp11::register]]
cpp11::writable::doubles kernel_quantile_dense_cpp(
    SEXP mat,
    int axis,
    double q,
    int threshold)
{
    const bool is_int = (TYPEOF(mat) == INTSXP);
    SEXP dim = Rf_getAttrib(mat, R_DimSymbol);
    const int nrow = INTEGER(dim)[0];
    const int ncol = INTEGER(dim)[1];

    const int* xint = is_int ? INTEGER_RO(mat) : nullptr;
    const double* xdbl = is_int ? nullptr : REAL_RO(mat);

    if (axis == 1) {
        // Per-column: output length = ncol.
        cpp11::writable::doubles out(ncol);
        double* pout = REAL(out.data());

        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            std::vector<double> buf;
            const int col_offset = j * nrow;
            const bool ok = is_int
                ? fill_col_int(xint, col_offset, nrow, buf)
                : fill_col_dbl(xdbl, col_offset, nrow, buf);
            pout[j] = ok ? quantile_inplace(buf, q) : NA_REAL;
        }
        return out;
    }

    // axis == 0: per-row: output length = nrow.
    cpp11::writable::doubles out(nrow);
    double* pout = REAL(out.data());

    DAFR_PARALLEL_FOR(nrow >= threshold)
    for (int r = 0; r < nrow; ++r) {
        std::vector<double> buf;
        const bool ok = is_int
            ? fill_row_int(xint, r, nrow, ncol, buf)
            : fill_row_dbl(xdbl, r, nrow, ncol, buf);
        pout[r] = ok ? quantile_inplace(buf, q) : NA_REAL;
    }
    return out;
}
```

- [ ] **Step 1.3: Regenerate cpp11 bindings and compile**

Run in R:

```r
setwd("~/src/dafr-native")
cpp11::cpp_register()
devtools::load_all()
```

Expected: no errors. `dafr:::kernel_quantile_dense_cpp` should be a callable closure.

- [ ] **Step 1.4: Write the failing unit-test file**

Create `tests/testthat/test-kernel-dense-quantile.R`:

```r
# tests/testthat/test-kernel-dense-quantile.R
# Correctness tests for kernel_quantile_dense_cpp (Slice 9c).
# The kernel accepts INTSXP or REALSXP and returns a doubles vector.

# ---------------------------------------------------------------------------
# Test 1: Double matrix, axis=1 (per-column), matches matrixStats::colQuantiles.
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense axis=1 double matches matrixStats::colQuantiles", {
    set.seed(201)
    m <- matrix(rnorm(100L * 50L), nrow = 100L, ncol = 50L)
    for (q in c(0, 0.25, 0.5, 0.75, 1.0)) {
        got <- dafr:::kernel_quantile_dense_cpp(m, axis = 1L, q = q, threshold = 1L)
        ref <- matrixStats::colQuantiles(m, probs = q, type = 7L, useNames = FALSE)
        expect_equal(got, as.numeric(ref), tolerance = sqrt(.Machine$double.eps),
                     label = sprintf("axis=1 double q=%g", q))
    }
})

# ---------------------------------------------------------------------------
# Test 2: Double matrix, axis=0 (per-row), matches matrixStats::rowQuantiles.
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense axis=0 double matches matrixStats::rowQuantiles", {
    set.seed(202)
    m <- matrix(rnorm(100L * 50L), nrow = 100L, ncol = 50L)
    for (q in c(0, 0.25, 0.5, 0.75, 1.0)) {
        got <- dafr:::kernel_quantile_dense_cpp(m, axis = 0L, q = q, threshold = 1L)
        ref <- matrixStats::rowQuantiles(m, probs = q, type = 7L, useNames = FALSE)
        expect_equal(got, as.numeric(ref), tolerance = sqrt(.Machine$double.eps),
                     label = sprintf("axis=0 double q=%g", q))
    }
})

# ---------------------------------------------------------------------------
# Test 3: Int-aware parity: kernel(int_mat, q) ~= kernel(as.double(int_mat), q).
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense int vs double parity", {
    set.seed(203)
    mi <- matrix(sample(0L:100L, 60L * 30L, replace = TRUE),
                 nrow = 60L, ncol = 30L)
    expect_true(is.integer(mi))
    md <- mi + 0.0
    for (q in c(0.1, 0.5, 0.9)) {
        got_int <- dafr:::kernel_quantile_dense_cpp(mi, axis = 1L, q = q, threshold = 1L)
        got_dbl <- dafr:::kernel_quantile_dense_cpp(md, axis = 1L, q = q, threshold = 1L)
        expect_equal(got_int, got_dbl, tolerance = sqrt(.Machine$double.eps),
                     label = sprintf("int-parity q=%g", q))
    }
})

# ---------------------------------------------------------------------------
# Test 4: Empty input -> NA_REAL (we define empty via zero-length dim).
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense empty column yields NA_REAL", {
    m <- matrix(double(0), nrow = 0L, ncol = 3L)
    got <- dafr:::kernel_quantile_dense_cpp(m, axis = 1L, q = 0.5, threshold = 1L)
    expect_length(got, 3L)
    expect_true(all(is.na(got)))
})

# ---------------------------------------------------------------------------
# Test 5: Single-value column returns that value at any q.
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense single-value column is identity", {
    m <- matrix(c(7.0, 42.0, -3.14), nrow = 1L, ncol = 3L)
    for (q in c(0, 0.25, 0.5, 0.75, 1.0)) {
        got <- dafr:::kernel_quantile_dense_cpp(m, axis = 1L, q = q, threshold = 1L)
        expect_equal(got, c(7.0, 42.0, -3.14), tolerance = 0)
    }
})

# ---------------------------------------------------------------------------
# Test 6: NaN in column yields NA_REAL for that column (strict semantics).
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense NaN column yields NA_REAL", {
    m <- matrix(c(1.0, 2.0, NaN, 4.0,     # col 1 has NaN
                  5.0, 6.0, 7.0, 8.0),    # col 2 clean
                nrow = 4L, ncol = 2L)
    got <- dafr:::kernel_quantile_dense_cpp(m, axis = 1L, q = 0.5, threshold = 1L)
    expect_true(is.na(got[1L]))
    expect_false(is.na(got[2L]))
    expect_equal(got[2L], 6.5, tolerance = sqrt(.Machine$double.eps))
})

# ---------------------------------------------------------------------------
# Test 7: NA_INTEGER in Int column yields NA_REAL.
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense NA_INTEGER column yields NA_REAL", {
    mi <- matrix(c(1L, 2L, NA_integer_, 4L,
                   5L, 6L, 7L, 8L),
                 nrow = 4L, ncol = 2L)
    got <- dafr:::kernel_quantile_dense_cpp(mi, axis = 1L, q = 0.5, threshold = 1L)
    expect_true(is.na(got[1L]))
    expect_false(is.na(got[2L]))
    expect_equal(got[2L], 6.5, tolerance = sqrt(.Machine$double.eps))
})

# ---------------------------------------------------------------------------
# Test 8: Parallelism invariance: output identical with threshold=1 vs threshold=huge.
# ---------------------------------------------------------------------------
test_that("kernel_quantile_dense output is threshold-invariant", {
    set.seed(204)
    m <- matrix(rnorm(80L * 40L), nrow = 80L, ncol = 40L)
    par  <- dafr:::kernel_quantile_dense_cpp(m, axis = 1L, q = 0.3, threshold = 1L)
    serial <- dafr:::kernel_quantile_dense_cpp(m, axis = 1L, q = 0.3,
                                               threshold = .Machine$integer.max)
    expect_identical(par, serial)
})
```

- [ ] **Step 1.5: Run tests to verify they pass**

Run in R:

```r
devtools::test(filter = "kernel-dense-quantile")
```

Expected: 8 `test_that` blocks, all PASS. Test summary `[ FAIL 0 | PASS ≥ 17 ]` (multiple expectations per block).

If any test fails, debug the kernel (most likely: NaN/NA_REAL distinction in `ISNAN`, or nth_element buffer reuse between lo and hi picks).

- [ ] **Step 1.6: Commit**

```bash
cd ~/src/dafr-native
git add src/kernel_quantile_dense.cpp \
        src/cpp11.cpp R/cpp11.R \
        tests/testthat/test-kernel-dense-quantile.R
git commit -m "$(cat <<'EOF'
perf(9c): dense Int-aware Quantile kernel

Adds kernel_quantile_dense_cpp: type-7 quantile on dense INTSXP or
REALSXP matrix without up-front storage-mode promotion. Mirrors
kernel_quantile_csc_cpp but without the neg/zero/pos implicit-zero
split. NaN or NA in a column short-circuits the column to NA_REAL.

Closes light-tier bake-off breach for julia_queries_026 (Quantile on
UMIs) pending R-dispatch wiring in Task 5.
EOF
)"
```

---

### Task 2: Dense Mode kernel

**Files:**
- Create: `src/kernel_mode_dense.cpp`
- Create: `tests/testthat/test-kernel-dense-mode.R`

Independent of Tasks 1 and 3 — safe to run in parallel.

- [ ] **Step 2.1: Read the template kernel**

Read `src/kernel_mode_csc.cpp` (203 lines). Note the `first_row` / `first_col` index tracking for the tiebreak. Drop the `first_zero_row` trick — dense has no implicit zeros.

- [ ] **Step 2.2: Write the kernel source file**

Create `src/kernel_mode_dense.cpp`:

```cpp
// src/kernel_mode_dense.cpp
// Per-column or per-row mode on a dense numeric matrix.
// Accepts INTSXP or REALSXP without up-front storage-mode copy.
//
// Matches .op_mode in R/operations.R bit-exactly:
//   ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]
// i.e., first-encountered wins on count ties.  Scan order = row index for
// axis=1, col index for axis=0.
//
// NA / NaN semantics (design spec §6):
//   - NaN values are routed to a single sentinel bucket (std::hash<double>
//     treats each NaN bit-pattern as a distinct key, and NaN != NaN makes
//     unordered_map unusable for NaN).  If the NaN bucket wins, output is NaN.
//   - NA_REAL is distinguished from other NaN via ISNA and bucketed separately
//     (can win as NA_REAL if most frequent).
//   - Empty input (n == 0) -> NA_REAL.
//
// axis == 1: per-column, output length = ncol
// axis == 0: per-row,    output length = nrow

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <cmath>
#include <limits>
#include <unordered_map>
#include <vector>

namespace {

// Separate sentinel for NaN (non-NA) and NA_REAL, because both are NaN-classed
// but have distinct semantics in R.  We bucket them in dedicated counters
// rather than inside the unordered_map (which can't hash NaN reliably).
struct ModeState {
    std::unordered_map<double, int> counts;
    std::unordered_map<double, int> first_seen;
    int nan_count      = 0;   // plain NaN (not NA)
    int nan_first_seen = std::numeric_limits<int>::max();
    int na_count       = 0;   // NA_REAL
    int na_first_seen  = std::numeric_limits<int>::max();
};

// Observe a value at scan position `pos` (row index for axis=1, col for axis=0).
inline void observe(ModeState& s, double v, int pos) {
    if (ISNA(v)) {
        s.na_count += 1;
        if (pos < s.na_first_seen) s.na_first_seen = pos;
        return;
    }
    if (std::isnan(v)) {
        s.nan_count += 1;
        if (pos < s.nan_first_seen) s.nan_first_seen = pos;
        return;
    }
    auto it = s.counts.find(v);
    if (it == s.counts.end()) {
        s.counts[v] = 1;
        s.first_seen[v] = pos;
    } else {
        it->second += 1;
    }
}

// Pick the mode: highest count wins; on ties, lowest first_seen wins.
// Buckets considered: all finite keys in `counts`, plus the nan and na sentinels.
double pick_mode(const ModeState& s, int n_observed) {
    if (n_observed == 0) return NA_REAL;
    double best_val = NA_REAL;
    int best_count = -1;
    int best_first = std::numeric_limits<int>::max();
    for (const auto& kv : s.counts) {
        const int cnt = kv.second;
        const int fs  = s.first_seen.at(kv.first);
        if (cnt > best_count || (cnt == best_count && fs < best_first)) {
            best_val = kv.first;
            best_count = cnt;
            best_first = fs;
        }
    }
    if (s.nan_count > best_count ||
        (s.nan_count == best_count && s.nan_first_seen < best_first)) {
        best_val = std::nan("");    // return a NaN (non-NA)
        best_count = s.nan_count;
        best_first = s.nan_first_seen;
    }
    if (s.na_count > best_count ||
        (s.na_count == best_count && s.na_first_seen < best_first)) {
        best_val = NA_REAL;
        best_count = s.na_count;
        best_first = s.na_first_seen;
    }
    return best_val;
}

inline double value_int(const int* xint, int idx) {
    const int vi = xint[idx];
    return (vi == NA_INTEGER) ? NA_REAL : static_cast<double>(vi);
}

inline double value_dbl(const double* xdbl, int idx) {
    return xdbl[idx];
}

} // namespace

[[cpp11::register]]
cpp11::writable::doubles kernel_mode_dense_cpp(
    SEXP mat,
    int axis,
    int threshold)
{
    const bool is_int = (TYPEOF(mat) == INTSXP);
    SEXP dim = Rf_getAttrib(mat, R_DimSymbol);
    const int nrow = INTEGER(dim)[0];
    const int ncol = INTEGER(dim)[1];

    const int* xint = is_int ? INTEGER_RO(mat) : nullptr;
    const double* xdbl = is_int ? nullptr : REAL_RO(mat);

    if (axis == 1) {
        cpp11::writable::doubles out(ncol);
        double* pout = REAL(out.data());

        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            ModeState s;
            const int col_offset = j * nrow;
            for (int i = 0; i < nrow; ++i) {
                const double v = is_int
                    ? value_int(xint, col_offset + i)
                    : value_dbl(xdbl, col_offset + i);
                observe(s, v, i);
            }
            pout[j] = pick_mode(s, nrow);
        }
        return out;
    }

    // axis == 0: per-row.
    cpp11::writable::doubles out(nrow);
    double* pout = REAL(out.data());

    DAFR_PARALLEL_FOR(nrow >= threshold)
    for (int r = 0; r < nrow; ++r) {
        ModeState s;
        for (int j = 0; j < ncol; ++j) {
            const double v = is_int
                ? value_int(xint, j * nrow + r)
                : value_dbl(xdbl, j * nrow + r);
            observe(s, v, j);
        }
        pout[r] = pick_mode(s, ncol);
    }
    return out;
}
```

- [ ] **Step 2.3: Regenerate cpp11 bindings and compile**

Run in R:

```r
setwd("~/src/dafr-native")
cpp11::cpp_register()
devtools::load_all()
```

Expected: clean compile. `dafr:::kernel_mode_dense_cpp` callable.

- [ ] **Step 2.4: Write the unit-test file**

Create `tests/testthat/test-kernel-dense-mode.R`:

```r
# tests/testthat/test-kernel-dense-mode.R
# Correctness tests for kernel_mode_dense_cpp (Slice 9c).
# The kernel accepts INTSXP or REALSXP; returns doubles vector.

# Authoritative reference: .op_mode in R/operations.R.
.op_mode_ref <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# ---------------------------------------------------------------------------
# Test 1: Double axis=1 (per-column) exact match vs apply(.op_mode).
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense axis=1 double matches apply(.op_mode)", {
    set.seed(301)
    m <- matrix(sample(c(-1.5, 0.0, 1.5, 2.5), 80L * 20L, replace = TRUE),
                nrow = 80L, ncol = 20L)
    got <- dafr:::kernel_mode_dense_cpp(m, axis = 1L, threshold = 1L)
    ref <- apply(m, 2L, .op_mode_ref)
    expect_identical(got, as.numeric(ref))
})

# ---------------------------------------------------------------------------
# Test 2: Double axis=0 (per-row) exact match.
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense axis=0 double matches apply(.op_mode)", {
    set.seed(302)
    m <- matrix(sample(c(-1.5, 0.0, 1.5, 2.5), 20L * 80L, replace = TRUE),
                nrow = 20L, ncol = 80L)
    got <- dafr:::kernel_mode_dense_cpp(m, axis = 0L, threshold = 1L)
    ref <- apply(m, 1L, .op_mode_ref)
    expect_identical(got, as.numeric(ref))
})

# ---------------------------------------------------------------------------
# Test 3: Tiebreak — first-seen row wins on count equality.
# Column c(1, 2, 1, 2): counts tie at 2; row 0 value 1 is first -> 1.
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense tie breaks on first-seen position", {
    m <- matrix(c(1.0, 2.0, 1.0, 2.0,
                  2.0, 1.0, 2.0, 1.0),
                nrow = 4L, ncol = 2L)
    got <- dafr:::kernel_mode_dense_cpp(m, axis = 1L, threshold = 1L)
    # col 1: first row = 1 -> mode = 1
    # col 2: first row = 2 -> mode = 2
    expect_identical(got, c(1.0, 2.0))
})

# ---------------------------------------------------------------------------
# Test 4: All-equal column and all-zeros column.
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense all-equal and all-zero columns", {
    m <- matrix(c(7.0, 7.0, 7.0, 7.0,
                  0.0, 0.0, 0.0, 0.0),
                nrow = 4L, ncol = 2L)
    got <- dafr:::kernel_mode_dense_cpp(m, axis = 1L, threshold = 1L)
    expect_identical(got, c(7.0, 0.0))
})

# ---------------------------------------------------------------------------
# Test 5: Int-aware parity: kernel(int_mat) == kernel(as.double(int_mat)).
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense int vs double parity", {
    set.seed(305)
    mi <- matrix(sample(0L:5L, 40L * 15L, replace = TRUE),
                 nrow = 40L, ncol = 15L)
    expect_true(is.integer(mi))
    md <- mi + 0.0
    got_int <- dafr:::kernel_mode_dense_cpp(mi, axis = 1L, threshold = 1L)
    got_dbl <- dafr:::kernel_mode_dense_cpp(md, axis = 1L, threshold = 1L)
    expect_identical(got_int, got_dbl)
})

# ---------------------------------------------------------------------------
# Test 6: NaN column — NaN bucketed as winnable mode.
# Column: NaN appears 3 times, 1.0 appears 2 times -> mode is NaN.
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense NaN bucket can win as mode", {
    m <- matrix(c(NaN, NaN, NaN, 1.0, 1.0,
                  1.0, 1.0, 1.0, NaN, NaN),
                nrow = 5L, ncol = 2L)
    got <- dafr:::kernel_mode_dense_cpp(m, axis = 1L, threshold = 1L)
    # col 1: NaN wins (3 vs 2).  col 2: 1.0 wins (3 vs 2).
    expect_true(is.nan(got[1L]))
    expect_false(is.na(got[1L]))   # NaN is not NA
    expect_equal(got[2L], 1.0)
})

# ---------------------------------------------------------------------------
# Test 7: NA_REAL bucketed separately from NaN.
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense NA_REAL bucket distinct from NaN", {
    m <- matrix(c(NA_real_, NA_real_, NA_real_, 1.0, 1.0), nrow = 5L, ncol = 1L)
    got <- dafr:::kernel_mode_dense_cpp(m, axis = 1L, threshold = 1L)
    expect_true(is.na(got[1L]))
    expect_false(is.nan(got[1L]))  # NA_REAL should be distinguishable
})

# ---------------------------------------------------------------------------
# Test 8: Empty column -> NA_REAL.
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense empty column yields NA_REAL", {
    m <- matrix(double(0), nrow = 0L, ncol = 3L)
    got <- dafr:::kernel_mode_dense_cpp(m, axis = 1L, threshold = 1L)
    expect_length(got, 3L)
    expect_true(all(is.na(got)))
})

# ---------------------------------------------------------------------------
# Test 9: Parallelism invariance.
# ---------------------------------------------------------------------------
test_that("kernel_mode_dense output is threshold-invariant", {
    set.seed(309)
    m <- matrix(sample(c(0.0, 1.0, 2.0), 100L * 50L, replace = TRUE),
                nrow = 100L, ncol = 50L)
    par    <- dafr:::kernel_mode_dense_cpp(m, axis = 1L, threshold = 1L)
    serial <- dafr:::kernel_mode_dense_cpp(m, axis = 1L,
                                           threshold = .Machine$integer.max)
    expect_identical(par, serial)
})
```

- [ ] **Step 2.5: Run tests to verify they pass**

```r
devtools::test(filter = "kernel-dense-mode")
```

Expected: 9 `test_that` blocks all PASS.

If Test 7 fails (NA_REAL ≡ NaN confusion), verify `ISNA(v)` is called *before* `std::isnan(v)` — `ISNA` is the more specific check.

- [ ] **Step 2.6: Commit**

```bash
cd ~/src/dafr-native
git add src/kernel_mode_dense.cpp \
        src/cpp11.cpp R/cpp11.R \
        tests/testthat/test-kernel-dense-mode.R
git commit -m "$(cat <<'EOF'
perf(9c): dense Int-aware Mode kernel

Adds kernel_mode_dense_cpp: mode with first-seen-position tiebreak
on dense INTSXP or REALSXP matrix. Matches .op_mode bit-exactly.
NaN and NA_REAL tracked in separate sentinel buckets (distinct from
finite-value unordered_map keys) per R semantics.

Closes light-tier bake-off breach for julia_queries_028 (Mode on
UMIs) pending R-dispatch wiring in Task 5.
EOF
)"
```

---

### Task 3: Grouped Min/Max dense kernel

**Files:**
- Create: `src/kernel_grouped_minmax_dense.cpp`
- Create: `tests/testthat/test-kernel-grouped-minmax-dense.R`

Independent of Tasks 1 and 2.

- [ ] **Step 3.1: Read the template kernel**

Read `src/kernel_grouped_rowsum_dense.cpp` (199 lines). Notice: single-threaded, `std::vector<bool>` NA-tracking, axis=2 (G2, ngroups × ncol) and axis=3 (G3, nrow × ngroups) mirror each other.

- [ ] **Step 3.2: Write the kernel source file**

Create `src/kernel_grouped_minmax_dense.cpp`:

```cpp
// src/kernel_grouped_minmax_dense.cpp
// Grouped Min or Max on a dense INTSXP/REALSXP matrix.  Sibling of
// kernel_grouped_rowsum_dense.cpp (Slice 9b).  Single-threaded, NA-safe.
//
// Design (spec §4.3):
//   - Accumulator init per (i, g): +inf (Min) or -inf (Max).
//   - First non-NA observation replaces sentinel; std::min/max thereafter.
//   - NA propagation: once a cell sees NA, stays NA.
//   - Empty group: sentinel remains; post-process in R dispatch layer
//     (.minmax_empty_to_na) strips sentinels to NA_REAL.
//
// Axis conventions:
//   axis == 2 (G2, row-grouped):  output is ngroups x ncol.
//   axis == 3 (G3, col-grouped):  output is nrow    x ngroups.
//
// Variant: 0 = Min, 1 = Max.

#include <cpp11.hpp>
#include <cmath>
#include <limits>
#include <vector>

using namespace cpp11;

[[cpp11::register]]
cpp11::writable::doubles_matrix<cpp11::by_column>
kernel_grouped_minmax_dense_cpp(
    SEXP mat,
    cpp11::integers groups,
    int ngroups,
    int axis,
    int variant)
{
    const int* pg = INTEGER(groups);
    const bool is_int = (TYPEOF(mat) == INTSXP);
    const bool is_max = (variant == 1);
    const double sentinel = is_max
        ? -std::numeric_limits<double>::infinity()
        :  std::numeric_limits<double>::infinity();

    SEXP dim = Rf_getAttrib(mat, R_DimSymbol);
    const int nrow = INTEGER(dim)[0];
    const int ncol = INTEGER(dim)[1];

    const int* xint = is_int ? INTEGER_RO(mat) : nullptr;
    const double* xdbl = is_int ? nullptr : REAL_RO(mat);

    if (axis == 2) {
        // G2: groups along rows -> output ngroups x ncol.
        cpp11::writable::doubles_matrix<cpp11::by_column> out(ngroups, ncol);
        std::vector<bool> na_flag(ngroups, false);

        for (int j = 0; j < ncol; ++j) {
            // Reset per-column state.
            std::fill(na_flag.begin(), na_flag.end(), false);
            for (int g = 0; g < ngroups; ++g) out(g, j) = sentinel;

            const int col_offset = j * nrow;
            for (int i = 0; i < nrow; ++i) {
                const int g = pg[i] - 1;
                double v;
                bool is_na;
                if (is_int) {
                    const int vi = xint[col_offset + i];
                    is_na = (vi == NA_INTEGER);
                    v = is_na ? 0.0 : static_cast<double>(vi);
                } else {
                    v = xdbl[col_offset + i];
                    is_na = ISNA(v);
                }

                if (na_flag[g]) continue;
                if (is_na) {
                    na_flag[g] = true;
                    out(g, j) = NA_REAL;
                    continue;
                }
                if (is_max) {
                    if (v > out(g, j)) out(g, j) = v;
                } else {
                    if (v < out(g, j)) out(g, j) = v;
                }
            }
        }
        return out;
    }

    // axis == 3: G3 -- groups along cols -> output nrow x ngroups.
    cpp11::writable::doubles_matrix<cpp11::by_column> out(nrow, ngroups);
    std::vector<bool> na_flag(static_cast<size_t>(nrow) * ngroups, false);

    // Initialize all output cells to sentinel.
    for (int g = 0; g < ngroups; ++g) {
        for (int i = 0; i < nrow; ++i) {
            out(i, g) = sentinel;
        }
    }

    for (int j = 0; j < ncol; ++j) {
        const int g = pg[j] - 1;
        const int col_offset = j * nrow;
        for (int i = 0; i < nrow; ++i) {
            double v;
            bool is_na;
            if (is_int) {
                const int vi = xint[col_offset + i];
                is_na = (vi == NA_INTEGER);
                v = is_na ? 0.0 : static_cast<double>(vi);
            } else {
                v = xdbl[col_offset + i];
                is_na = ISNA(v);
            }

            const size_t idx = static_cast<size_t>(g) * nrow + i;
            if (na_flag[idx]) continue;
            if (is_na) {
                na_flag[idx] = true;
                out(i, g) = NA_REAL;
                continue;
            }
            if (is_max) {
                if (v > out(i, g)) out(i, g) = v;
            } else {
                if (v < out(i, g)) out(i, g) = v;
            }
        }
    }
    return out;
}
```

- [ ] **Step 3.3: Regenerate cpp11 bindings and compile**

```r
setwd("~/src/dafr-native")
cpp11::cpp_register()
devtools::load_all()
```

Expected: clean compile.

- [ ] **Step 3.4: Write the unit-test file**

Create `tests/testthat/test-kernel-grouped-minmax-dense.R`:

```r
# tests/testthat/test-kernel-grouped-minmax-dense.R
# Correctness tests for kernel_grouped_minmax_dense_cpp (Slice 9c).

# Authoritative reference builders (match the pre-slice R fallback at
# R/query_eval.R:1104-1122 and L1152-1170).
.ref_g2 <- function(m, gi, ngroups, fn) {
    idx <- split(seq_len(nrow(m)), gi)
    out <- matrix(0, ngroups, ncol(m))
    for (g in seq_len(ngroups)) {
        if (length(idx[[g]]) == 0L) { out[g, ] <- NA_real_; next }
        out[g, ] <- fn(m[idx[[g]], , drop = FALSE])
    }
    out
}
.ref_g3 <- function(m, gi, ngroups, fn) {
    idx <- split(seq_len(ncol(m)), gi)
    out <- matrix(0, nrow(m), ngroups)
    for (g in seq_len(ngroups)) {
        if (length(idx[[g]]) == 0L) { out[, g] <- NA_real_; next }
        out[, g] <- fn(m[, idx[[g]], drop = FALSE])
    }
    out
}

# ---------------------------------------------------------------------------
# Test 1: G2 Max (axis=2) on integer matrix.
# Note: the kernel returns +Inf / -Inf for empty groups; the R dispatch
# layer post-processes to NA_REAL via .minmax_empty_to_na.  We test the
# kernel output pre-post-process here.
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax G2 Max integer matches per-group colMaxs", {
    set.seed(401)
    m <- matrix(sample(0L:20L, 30L, replace = TRUE), nrow = 6L, ncol = 5L)
    expect_true(is.integer(m))
    gi <- c(1L, 1L, 2L, 2L, 3L, 3L)
    ngroups <- 3L
    got <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, groups = gi, ngroups = ngroups, axis = 2L, variant = 1L)
    expected <- .ref_g2(m, gi, ngroups, matrixStats::colMaxs)
    expect_equal(unname(got), unname(expected + 0.0), tolerance = 0)
})

# ---------------------------------------------------------------------------
# Test 2: G2 Min (axis=2).
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax G2 Min integer matches per-group colMins", {
    set.seed(402)
    m <- matrix(sample(0L:20L, 30L, replace = TRUE), nrow = 6L, ncol = 5L)
    gi <- c(1L, 1L, 2L, 2L, 3L, 3L)
    ngroups <- 3L
    got <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, gi, ngroups, axis = 2L, variant = 0L)
    expected <- .ref_g2(m, gi, ngroups, matrixStats::colMins)
    expect_equal(unname(got), unname(expected + 0.0), tolerance = 0)
})

# ---------------------------------------------------------------------------
# Test 3: G3 Max (axis=3).
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax G3 Max integer matches per-group rowMaxs", {
    set.seed(403)
    m <- matrix(sample(0L:20L, 20L, replace = TRUE), nrow = 4L, ncol = 5L)
    gi <- c(1L, 2L, 1L, 2L, 1L)
    ngroups <- 2L
    got <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, gi, ngroups, axis = 3L, variant = 1L)
    expected <- .ref_g3(m, gi, ngroups, matrixStats::rowMaxs)
    expect_equal(unname(got), unname(expected + 0.0), tolerance = 0)
})

# ---------------------------------------------------------------------------
# Test 4: G3 Min (axis=3).
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax G3 Min integer matches per-group rowMins", {
    set.seed(404)
    m <- matrix(sample(0L:20L, 20L, replace = TRUE), nrow = 4L, ncol = 5L)
    gi <- c(1L, 2L, 1L, 2L, 1L)
    ngroups <- 2L
    got <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, gi, ngroups, axis = 3L, variant = 0L)
    expected <- .ref_g3(m, gi, ngroups, matrixStats::rowMins)
    expect_equal(unname(got), unname(expected + 0.0), tolerance = 0)
})

# ---------------------------------------------------------------------------
# Test 5: Int vs double parity, all four variants.
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax int vs double parity, all variants", {
    set.seed(405)
    mi <- matrix(sample(0L:50L, 6L * 5L, replace = TRUE), nrow = 6L, ncol = 5L)
    expect_true(is.integer(mi))
    md <- mi + 0.0
    gi2 <- c(1L, 1L, 2L, 2L, 3L, 3L); ngroups2 <- 3L
    gi3 <- c(1L, 2L, 1L, 2L, 1L);     ngroups3 <- 2L
    for (axis in c(2L, 3L)) {
        for (variant in c(0L, 1L)) {
            gi      <- if (axis == 2L) gi2 else gi3
            ngroups <- if (axis == 2L) ngroups2 else ngroups3
            got_int <- dafr:::kernel_grouped_minmax_dense_cpp(
                mi, gi, ngroups, axis = axis, variant = variant)
            got_dbl <- dafr:::kernel_grouped_minmax_dense_cpp(
                md, gi, ngroups, axis = axis, variant = variant)
            expect_identical(got_int, got_dbl,
                label = sprintf("axis=%d variant=%d", axis, variant))
        }
    }
})

# ---------------------------------------------------------------------------
# Test 6: NA propagation — NA in one cell makes (row, group) output NA.
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax NA propagation per (row, group)", {
    m <- matrix(c(1L, 2L, 3L,    # row 1: group 1
                  4L, 5L, 6L,    # row 2: group 2
                  7L, 8L, 9L,    # row 3: group 1
                  10L, 11L, 12L),
                nrow = 4L, ncol = 3L)
    m[1L, 2L] <- NA_integer_     # row 1 col 2 belongs to group 1
    gi <- c(1L, 2L, 1L, 2L); ngroups <- 2L
    got <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, gi, ngroups, axis = 2L, variant = 1L)  # G2 Max
    # col 2: group 1 aggregated rows {1, 3} -> row 1 is NA -> NA.
    expect_true(is.na(got[1L, 2L]))
    expect_false(is.na(got[2L, 2L]))
    expect_false(is.na(got[1L, 1L]))
})

# ---------------------------------------------------------------------------
# Test 7: Empty group — sentinel remains (+Inf or -Inf) pre-post-process.
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax empty group produces sentinel", {
    m <- matrix(1:12 + 0.0, nrow = 4L, ncol = 3L)
    gi <- c(1L, 1L, 3L, 3L); ngroups <- 3L   # group 2 is empty
    got_max <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, gi, ngroups, axis = 2L, variant = 1L)
    got_min <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, gi, ngroups, axis = 2L, variant = 0L)
    expect_true(all(got_max[2L, ] == -Inf))
    expect_true(all(got_min[2L, ] ==  Inf))
})

# ---------------------------------------------------------------------------
# Test 8: Single-element group — that element for every position.
# ---------------------------------------------------------------------------
test_that("kernel_grouped_minmax single-element group returns that element", {
    m <- matrix(c(10.0, 20.0,
                  30.0, 40.0,
                  50.0, 60.0), nrow = 3L, ncol = 2L)
    gi <- c(1L, 2L, 3L); ngroups <- 3L
    got <- dafr:::kernel_grouped_minmax_dense_cpp(
        m, gi, ngroups, axis = 2L, variant = 1L)  # G2 Max
    # Each group has one row; the max equals that row.
    expect_equal(got[1L, ], c(10.0, 20.0))
    expect_equal(got[2L, ], c(30.0, 40.0))
    expect_equal(got[3L, ], c(50.0, 60.0))
})
```

- [ ] **Step 3.5: Run tests to verify they pass**

```r
devtools::test(filter = "kernel-grouped-minmax-dense")
```

Expected: 8 `test_that` blocks all PASS.

- [ ] **Step 3.6: Commit**

```bash
cd ~/src/dafr-native
git add src/kernel_grouped_minmax_dense.cpp \
        src/cpp11.cpp R/cpp11.R \
        tests/testthat/test-kernel-grouped-minmax-dense.R
git commit -m "$(cat <<'EOF'
perf(9c): dense Int-aware grouped Min/Max kernel

Adds kernel_grouped_minmax_dense_cpp: per-(row, group) min or max on
dense INTSXP or REALSXP matrix with NA propagation ('once NA, always
NA' per (i, g)). Single-threaded, matches kernel_grouped_rowsum_dense
pattern. Empty groups retain +Inf / -Inf sentinel for R-layer
post-processing to NA_REAL.

Closes light-tier bake-off breaches for julia_queries_043 (G2 Max)
and _047 (G3 Max) pending R-dispatch wiring in Task 5.
EOF
)"
```

---

### Task 4: Merge + cpp11 regen + kernel-only validation

**Files:**
- Regenerate: `src/cpp11.cpp`
- Regenerate: `R/cpp11.R`

**Depends on:** Tasks 1, 2, 3 all complete and merged to the working branch.

- [ ] **Step 4.1: Merge the three kernel branches**

If Tasks 1-3 were executed in separate worktrees, merge them into the Slice 9c branch. Conflicts in `src/cpp11.cpp` and `R/cpp11.R` are expected — they are auto-generated and each task rewrote them. Resolve by accepting one version or taking theirs, then regenerate in Step 4.2.

```bash
cd ~/src/dafr-native
# If conflicts in cpp11.cpp or R/cpp11.R:
git checkout --theirs src/cpp11.cpp R/cpp11.R
git add src/cpp11.cpp R/cpp11.R
# Any other conflicts should not exist if tasks respected file boundaries.
git commit  # finish the merge
```

- [ ] **Step 4.2: Regenerate cpp11 on the merged source tree**

Run in R:

```r
setwd("~/src/dafr-native")
cpp11::cpp_register()
```

Expected: `src/cpp11.cpp` and `R/cpp11.R` are overwritten with bindings for all three new kernels PLUS all pre-existing kernels.

- [ ] **Step 4.3: Verify all three new entry points are registered**

```bash
grep -c 'kernel_quantile_dense_cpp\|kernel_mode_dense_cpp\|kernel_grouped_minmax_dense_cpp' src/cpp11.cpp
```

Expected output: `6` (each symbol appears twice — once in the forward decl, once in the `extern "C"` wrapper).

```bash
grep -c 'kernel_quantile_dense_cpp\|kernel_mode_dense_cpp\|kernel_grouped_minmax_dense_cpp' R/cpp11.R
```

Expected: `6` (each R thunk has its name on one line and `.Call` on another).

- [ ] **Step 4.4: Clean build and reload**

```r
setwd("~/src/dafr-native")
devtools::clean_dll()
devtools::load_all()
```

Expected: clean compile, no warnings. All three kernels available as `dafr:::kernel_*_dense_cpp`.

- [ ] **Step 4.5: Run the three new test files + the existing 9b kernel tests**

```r
devtools::test(filter = "kernel-dense-quantile|kernel-dense-mode|kernel-grouped-minmax-dense|kernel-grouped-rowsum")
```

Expected: all three new test files pass (25+ expectations total), and the 9b rowsum tests still pass (no regression from the merge/regen).

- [ ] **Step 4.6: Run full test suite to catch integration regressions**

```r
devtools::test()
```

Expected: `[ FAIL 0 | WARN 1 | SKIP 1 | PASS ≥ 1840 ]`. New kernel tests add ~25 passes, so target is `≥ 1865`. The dispatch layer is not yet wired, so existing query tests should behave identically to pre-slice.

- [ ] **Step 4.7: Commit the regenerated bindings if merge didn't already**

If Step 4.2 produced changes beyond what the merge already committed:

```bash
git add src/cpp11.cpp R/cpp11.R
git commit -m "perf(9c): regenerate cpp11 bindings for dense kernels"
```

---

### Task 5: Wire R dispatch layer

**Files:**
- Modify: `R/query_eval.R` (4 dispatch sites + 1 helper addition)

**Depends on:** Task 4 complete.

- [ ] **Step 5.1: Add the `.minmax_empty_to_na` helper**

Locate the existing `.grouped_dense_rowsum` function in `R/query_eval.R` (around line 1065). Add the helper just above it:

```r
# Scrub +/-Inf sentinels from empty groups in kernel_grouped_minmax_dense output.
# axis = 2L: output is ngroups x ncol -> zero-count groups = NA rows.
# axis = 3L: output is nrow x ngroups -> zero-count groups = NA cols.
.minmax_empty_to_na <- function(out, gi, ngroups, axis) {
    empty <- tabulate(gi, ngroups) == 0L
    if (!any(empty)) return(out)
    if (axis == 2L) {
        out[empty, ] <- NA_real_
    } else {
        out[, empty] <- NA_real_
    }
    out
}
```

- [ ] **Step 5.2: Replace the per-row Quantile / Mode dispatch (ReduceToColumn)**

Find `R/query_eval.R:671–688` (the `Quantile = { ... }, Mode = ...` block inside the ReduceToColumn dispatch — the one with `axis = 0L`).

**Current:**
```r
Quantile = {
    q <- .param_quantile_q(params)
    if (is_dg)
        kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
            axis = 0L, q = q, threshold = .dafr_kernel_threshold())
    else if (is_dense)
        matrixStats::rowQuantiles(m, probs = q, type = 7L,
            useNames = FALSE)
    else return(NULL)
},
Mode = if (is_dg && is.numeric(m@x))
    kernel_mode_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
        axis = 0L, threshold = .dafr_kernel_threshold())
else if (is_dense && is.numeric(m))
    apply(m, 1L, function(v) op_mode_fn(v))
else return(NULL),
```

**Replace with:**
```r
Quantile = {
    q <- .param_quantile_q(params)
    if (is_dg)
        kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
            axis = 0L, q = q, threshold = .dafr_kernel_threshold())
    else if (is_dense && is.numeric(m))
        kernel_quantile_dense_cpp(m, axis = 0L, q = q,
            threshold = .dafr_kernel_threshold())
    else return(NULL)
},
Mode = if (is_dg && is.numeric(m@x))
    kernel_mode_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
        axis = 0L, threshold = .dafr_kernel_threshold())
else if (is_dense && is.numeric(m))
    kernel_mode_dense_cpp(m, axis = 0L,
        threshold = .dafr_kernel_threshold())
else return(NULL),
```

- [ ] **Step 5.3: Replace the per-col Quantile / Mode dispatch (ReduceToRow)**

Find `R/query_eval.R:773–789` (`axis = 1L` symmetric block).

**Current (relevant section):**
```r
Quantile = {
    q <- .param_quantile_q(params)
    if (is_dg)
        kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
            axis = 1L, q = q, threshold = .dafr_kernel_threshold())
    else if (is_dense)
        matrixStats::colQuantiles(m, probs = q, type = 7L,
            useNames = FALSE)
    else return(NULL)
},
Mode = if (is_dg && is.numeric(m@x))
    kernel_mode_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
        axis = 1L, threshold = .dafr_kernel_threshold())
else if (is_dense && is.numeric(m))
    apply(m, 2L, function(v) op_mode_fn(v))
else return(NULL),
```

**Replace with:**
```r
Quantile = {
    q <- .param_quantile_q(params)
    if (is_dg)
        kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
            axis = 1L, q = q, threshold = .dafr_kernel_threshold())
    else if (is_dense && is.numeric(m))
        kernel_quantile_dense_cpp(m, axis = 1L, q = q,
            threshold = .dafr_kernel_threshold())
    else return(NULL)
},
Mode = if (is_dg && is.numeric(m@x))
    kernel_mode_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
        axis = 1L, threshold = .dafr_kernel_threshold())
else if (is_dense && is.numeric(m))
    kernel_mode_dense_cpp(m, axis = 1L,
        threshold = .dafr_kernel_threshold())
else return(NULL),
```

Note: `op_mode_fn` local alias (defined around `R/query_eval.R:575`) is no longer referenced by either dispatch site. Search `grep -n op_mode_fn R/query_eval.R` — if no references remain, delete the alias line; otherwise leave it.

- [ ] **Step 5.4: Replace the G2 Max/Min grouped dispatch**

Find `R/query_eval.R:1104–1122` inside `.grouped_dense_rowsum` (axis == 2L branch).

**Current:**
```r
switch(label,
    Max = {
        idx <- split(seq_len(nrow(m)), gi)
        out <- matrix(0, ngroups, ncol(m))
        for (g in seq_len(ngroups)) {
            if (length(idx[[g]]) == 0L) next
            out[g, ] <- matrixStats::colMaxs(m[idx[[g]], , drop = FALSE])
        }
        out
    },
    Min = {
        idx <- split(seq_len(nrow(m)), gi)
        out <- matrix(0, ngroups, ncol(m))
        for (g in seq_len(ngroups)) {
            if (length(idx[[g]]) == 0L) next
            out[g, ] <- matrixStats::colMins(m[idx[[g]], , drop = FALSE])
        }
        out
    },
    # Fallback for unknown ops
    kernel_grouped_reduce_dense_cpp(
        m, group = gi, ngroups = ngroups,
        n_in_group = n_in_group, axis = axis,
        op = label, eps = eps,
        threshold = .dafr_kernel_threshold())
)
```

**Replace with:**
```r
switch(label,
    Max = .minmax_empty_to_na(
        kernel_grouped_minmax_dense_cpp(m, groups = gi, ngroups = ngroups,
            axis = 2L, variant = 1L),
        gi, ngroups, axis = 2L),
    Min = .minmax_empty_to_na(
        kernel_grouped_minmax_dense_cpp(m, groups = gi, ngroups = ngroups,
            axis = 2L, variant = 0L),
        gi, ngroups, axis = 2L),
    # Fallback for unknown ops
    kernel_grouped_reduce_dense_cpp(
        m, group = gi, ngroups = ngroups,
        n_in_group = n_in_group, axis = axis,
        op = label, eps = eps,
        threshold = .dafr_kernel_threshold())
)
```

- [ ] **Step 5.5: Replace the G3 Max/Min grouped dispatch**

Find `R/query_eval.R:1152–1170` (axis == 3L branch).

**Current:**
```r
switch(label,
    Max = {
        idx <- split(seq_len(ncol(m)), gi)
        out <- matrix(0, nrow(m), ngroups)
        for (g in seq_len(ngroups)) {
            if (length(idx[[g]]) == 0L) next
            out[, g] <- matrixStats::rowMaxs(m[, idx[[g]], drop = FALSE])
        }
        out
    },
    Min = {
        idx <- split(seq_len(ncol(m)), gi)
        out <- matrix(0, nrow(m), ngroups)
        for (g in seq_len(ngroups)) {
            if (length(idx[[g]]) == 0L) next
            out[, g] <- matrixStats::rowMins(m[, idx[[g]], drop = FALSE])
        }
        out
    },
    # Fallback for unknown ops
    kernel_grouped_reduce_dense_cpp(
        m, group = gi, ngroups = ngroups,
        n_in_group = n_in_group, axis = axis,
        op = label, eps = eps,
        threshold = .dafr_kernel_threshold())
)
```

**Replace with:**
```r
switch(label,
    Max = .minmax_empty_to_na(
        kernel_grouped_minmax_dense_cpp(m, groups = gi, ngroups = ngroups,
            axis = 3L, variant = 1L),
        gi, ngroups, axis = 3L),
    Min = .minmax_empty_to_na(
        kernel_grouped_minmax_dense_cpp(m, groups = gi, ngroups = ngroups,
            axis = 3L, variant = 0L),
        gi, ngroups, axis = 3L),
    # Fallback for unknown ops
    kernel_grouped_reduce_dense_cpp(
        m, group = gi, ngroups = ngroups,
        n_in_group = n_in_group, axis = axis,
        op = label, eps = eps,
        threshold = .dafr_kernel_threshold())
)
```

- [ ] **Step 5.6: Reload and run the full test suite**

```r
setwd("~/src/dafr-native")
devtools::load_all()
devtools::test()
```

Expected: `[ FAIL 0 | WARN 1 | SKIP 1 | PASS ≥ 1865 ]`. If any `test-query-*.R` regression test fails, compare output of the new kernel vs the old fallback on the offending input — most likely cause is a semantic gap between matrixStats and our kernel (e.g., different NA handling or empty-group behavior).

- [ ] **Step 5.7: Run R CMD check locally**

```r
setwd("~/src/dafr-native")
devtools::check(error_on = "warning")
```

Expected: 0 errors, 0 warnings, ≤ 4 notes (same as 9b baseline: benchmarks dir, installed size, future timestamps, hidden `.claude/`).

- [ ] **Step 5.8: Commit**

```bash
cd ~/src/dafr-native
git add R/query_eval.R
git commit -m "$(cat <<'EOF'
perf(9c): wire dense Int-aware Quantile/Mode/MinMax kernels

Replaces four dense-layout dispatch branches in R/query_eval.R:
  - Quantile / Mode at ReduceToColumn (axis=0) and ReduceToRow (axis=1)
  - Grouped Max / Min at G2 (axis=2) and G3 (axis=3) in
    .grouped_dense_rowsum

Adds .minmax_empty_to_na helper to strip +Inf / -Inf sentinels from
empty groups (the kernel leaves sentinels in place for performance;
the post-process runs only if any group is empty).

Closes the 4 remaining dense-path bake-off breaches pending final
benchmark re-run in Task 6.
EOF
)"
```

---

### Task 6: Bake-off validation

**Files:**
- Append: `dev/benchmarks/perf-log.md`
- Append: `benchmarks/perf-log.md` (if it exists; check first)
- Create: `dev/benchmarks/2026-04-22-post-slice-9c/` (results archive)

**Depends on:** Task 5 complete.

- [ ] **Step 6.1: Confirm the bake-off harness still calls `empty_cache` per iteration**

This is a load-bearing invariant flagged in the kickoff. Before any bench run:

```bash
grep -n empty_cache ~/src/dafr-native/benchmarks/R/run-bakeoff.R
```

Expected: at least one hit — a call inside the benchmarked loop, not before it. Also check the Julia side:

```bash
grep -n 'empty_cache\!' ~/src/dafr-native/benchmarks/julia/run_bakeoff.jl
```

Expected: at least one hit. If either is missing, STOP and re-read Slice 9b commit `2cc1348` before proceeding.

- [ ] **Step 6.2: Run the R-side benchmark**

```bash
cd ~/src/dafr-native
Rscript benchmarks/R/run-bakeoff.R --out /tmp/slice-9c-r-times.csv
```

Expected runtime: ~1-2 minutes for 79 queries.

- [ ] **Step 6.3: Run the Julia-side benchmark**

```bash
conda activate dafr-mcview
julia --project=benchmarks/julia benchmarks/julia/run_bakeoff.jl --out /tmp/slice-9c-julia-times.csv
```

Expected runtime: similar.

- [ ] **Step 6.4: Run the comparison**

```bash
mkdir -p ~/src/dafr-native/dev/benchmarks/2026-04-22-post-slice-9c
cd ~/src/dafr-native
Rscript benchmarks/compare.R \
    --r /tmp/slice-9c-r-times.csv \
    --julia /tmp/slice-9c-julia-times.csv \
    --out dev/benchmarks/2026-04-22-post-slice-9c/report.md
cp /tmp/slice-9c-r-times.csv /tmp/slice-9c-julia-times.csv \
   dev/benchmarks/2026-04-22-post-slice-9c/
```

- [ ] **Step 6.5: Inspect the report for the four target queries**

```bash
grep -E 'julia_queries_(026|028|043|047)' dev/benchmarks/2026-04-22-post-slice-9c/report.md
```

**Pass condition:** all four ratios ≤ 1.5× (hard threshold: ≤ 2.0× light-tier).

**Fail condition:** any above 2.0× → kernel bug, revisit. If between 1.5× and 2.0× → investigate but probably shippable (flicker territory).

- [ ] **Step 6.6: Check for regressions on the other 75 queries**

```bash
grep -c '^| ' dev/benchmarks/2026-04-22-post-slice-9c/report.md   # sanity check
# Manual: scan the comparison report for any query that was < tier-threshold
# pre-9c but is > tier-threshold now.
```

Expected: no new breaches on the other 75 queries. Light-tier threshold is 2.0×; heavy-tier is 1.5×. Compare against `dev/benchmarks/perf-log.md` for pre-9c ratios.

- [ ] **Step 6.7: Append the perf-log entry**

Open `dev/benchmarks/perf-log.md` and append (matching the 9b entry style):

```markdown
## Slice 9c (2026-04-22, commit <HEAD-sha>)

Delta from Slice 9b baseline: 4 queries closed via dense Int-aware
C++ kernels.

| Query | Op | Before (9b) | After (9c) | Close |
|---|---|---|---|---|
| julia_queries_026 | Quantile | 3.00× | <NEW>× | kernel_quantile_dense |
| julia_queries_028 | Mode | 2.27× | <NEW>× | kernel_mode_dense |
| julia_queries_043 | G2 Max | 2.16× | <NEW>× | kernel_grouped_minmax_dense |
| julia_queries_047 | G3 Max | 2.14× | <NEW>× | kernel_grouped_minmax_dense |

Remaining breach count: 7 -> 3 (all mmap S7-ctor floor; deferred to 9d+).

Artifacts: `dev/benchmarks/2026-04-22-post-slice-9c/`.
```

Fill `<NEW>` and `<HEAD-sha>` with real values from Step 6.5 and `git rev-parse --short HEAD`.

Also append a parallel entry to `benchmarks/perf-log.md` if that file exists in the package repo.

- [ ] **Step 6.8: Commit perf-log updates**

```bash
# Dev repo (separate git):
cd ~/src/dafr-native/dev
git add benchmarks/perf-log.md \
        benchmarks/2026-04-22-post-slice-9c/ \
        notes/2026-04-22-slice-9c-design.md \
        plans/2026-04-22-slice-9c-perf-closure.md
git commit -m "slice 9c: bake-off closure + design/plan artifacts"

# Package repo:
cd ~/src/dafr-native
if [ -f benchmarks/perf-log.md ]; then
    git add benchmarks/perf-log.md
    git commit -m "perf(9c): perf-log entry for 4-query dense-kernel closure"
fi
```

---

### Task 7: NEWS entry + slice exit

**Files:**
- Modify: `NEWS.md`
- Create: `dev/notes/slice-9c-exit.md`

**Depends on:** Task 6 complete and passing.

- [ ] **Step 7.1: Add the NEWS.md bullet**

Open `NEWS.md`. The file starts with `# dafr (development version)` followed by the existing `## Slice 9b — …` section with `### Performance` / `### Tooling` subheaders. Insert a new `## Slice 9c` block **above** the 9b block (newest-first chronology):

```markdown
## Slice 9c — Dense perf closure (2026-04-22)

### Performance

* **Dense Int-aware Quantile, Mode, and grouped Min/Max kernels.** Three
  new cpp11 kernels — `kernel_quantile_dense_cpp`, `kernel_mode_dense_cpp`,
  `kernel_grouped_minmax_dense_cpp` — replace the prior
  `matrixStats::colQuantiles` / `apply(.op_mode)` /
  `matrixStats::rowMaxs`-in-loop paths for dense-layout queries on Int32
  matrices. All three accept INTSXP or REALSXP directly, avoiding the
  `storage.mode(m) <- "double"` copy that dominated light-tier query
  time on the 856 × 683 UMIs mmap matrix. Closes the 4 remaining
  bake-off breaches against DAF.jl (`julia_queries_026` Quantile,
  `_028` Mode, `_043` G2 Max, `_047` G3 Max); the remaining 3 accepted
  breaches are the mmap-query S7-ctor floor (deferred).
```

- [ ] **Step 7.2: Commit NEWS**

```bash
cd ~/src/dafr-native
git add NEWS.md
git commit -m "docs(9c): NEWS entry for dense perf closure"
```

- [ ] **Step 7.3: Write the slice exit note**

Create `dev/notes/slice-9c-exit.md` using `slice-9b-exit.md` as the template. Cover:

- What shipped (3 kernels, 4 dispatch edits, 25+ new tests, NEWS, perf-log).
- Final test count (from Step 5.6).
- Bake-off numbers (from Step 6.5) + final breach count summary table.
- Any surprises or follow-ups discovered during implementation.
- Julia DAF state at exit (verify `~/src/DataAxesFormats.jl` commit hash is still `49fbba140437387a378217c2fa658d4231d0c8c1` — unchanged since Slice 3).
- Artifacts list.

- [ ] **Step 7.4: Commit the exit note**

```bash
cd ~/src/dafr-native/dev
git add notes/slice-9c-exit.md
git commit -m "slice 9c: exit note"
```

- [ ] **Step 7.5: Final package-repo summary commit (if desired)**

If the slice used a feature branch (e.g., `slice-9c-perf-closure`), this is the point to merge into `main`. The user will typically review before merging — do not auto-merge without a "ship it" approval.

---

## Acceptance Criteria (slice-level)

The slice is complete when ALL of the following are true:

1. **Tests:** `devtools::test()` on `main` after merge reports `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 1865–1880 ]` (range allows for author-style variation in how many assertions land per test block).
2. **Check:** `devtools::check(error_on = "warning")` reports 0 errors, 0 warnings, ≤ 4 notes (same notes as Slice 9b).
3. **Bake-off:** `dev/benchmarks/2026-04-22-post-slice-9c/report.md` shows all four target queries (026, 028, 043, 047) at ≤ 1.5× (or at minimum ≤ 2.0×), and no other query regressed above its tier threshold.
4. **CI:** On push to origin, GitHub Actions ubuntu/macos/windows + altrep-sanity all green. (CI run is not a step above — check it manually or via `gh run watch` after the push.)
5. **Documentation:** `NEWS.md` has the Slice 9c bullet. `dev/notes/slice-9c-exit.md` summarizes the slice. `dev/benchmarks/perf-log.md` has the slice entry.
