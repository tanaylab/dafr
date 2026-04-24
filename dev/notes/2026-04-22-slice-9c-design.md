# Slice 9c — Design: Dense Int-aware Perf Kernels

**Date:** 2026-04-22
**Predecessor:** Slice 9b (tag `slice-9b` on `main` at `9ab46e5`).
**Kickoff:** `dev/notes/slice-9c-kickoff.md`.
**Bundle:** Option P (Perf closure), items 1 + 2 from the kickoff menu.

## 1. Goal

Close the 4 remaining dense-path bake-off breaches on the `cells_daf`
fixture by adding C++ kernels that eliminate the Int32→double promotion
tax and move the `matrixStats::rowMaxs` / `apply(op_mode)` loops into
native code.

Breaches targeted (from `dev/benchmarks/perf-log.md`, post-9b state):

| Query | Op | Tier | Current | Target |
|---|---|---|---|---|
| `julia_queries_026` | Quantile on UMIs (dense, non-grouped) | light | 3.00× | ≤ 1.5× |
| `julia_queries_028` | Mode on UMIs (dense, non-grouped) | light | 2.27× | ≤ 1.5× |
| `julia_queries_043` | G2 Max grouped (dense) | light | 2.16× | ≤ 1.5× |
| `julia_queries_047` | G3 Max grouped (dense) | light | 2.14× | ≤ 1.5× |

**Done signal:** all four within light-query tier threshold (≤ 2.0×)
with headroom, no regressions on the other 75 queries, full test
suite green (target 1870–1880 passes, 0 fails).

## 2. Out of scope

- Median on dense — not breached in 9b; stays on
  `matrixStats::colMedians`/`rowMedians`.
- Any CSC-layout kernels — CSC Quantile/Mode/MinMax already ship.
- G3 kernel memory fix (Option M, deferred to 9d+ after
  128-thread lab-machine profile).
- `copy_all` double-write bug (deferred).
- mmap-open structural floor (5 accepted "R-dispatch floor"
  breaches remain).

## 3. Locked decisions

The following were locked during brainstorming:

1. **Bundle:** Option P, shipping item 1 (Quantile + Mode dense
   kernels) *and* item 2 (grouped Min/Max dense kernel) together.
2. **Int-awareness:** new dense kernels accept INTSXP or REALSXP
   directly (Option A from brainstorm Q3), matching the 9b rowsum
   kernel's pattern. This is the primary driver — the `storage.mode(m)
   <- "double"` cost on a 856×683 Int32 mmap'd UMIs matrix is ~2 ms,
   which is most of the remaining gap.
3. **Item 2 organization:** sibling kernel
   `kernel_grouped_minmax_dense.cpp` (Option A from brainstorm Q4).
   Rationale: accumulator math is genuinely different from Sum/SumSq;
   the 9b kernel is freshly landed and not worth churning; mirrors the
   CSC side (`kernel_minmax_csc.cpp` is a sibling of `kernel_var_csc`
   and `kernel_quantile_csc`).
4. **Median:** out of scope (Option B from brainstorm Q5). Scope
   discipline — `matrixStats::colMedians` is well-tuned C and not
   flagged as a breach.
5. **NaN semantics:** column with any NaN → column's output is
   `NA_REAL` for Quantile; NaN bucketed as-is for Mode (can win).
   Documented contract; matches `matrixStats::colQuantiles` default
   (`na.rm=FALSE`).
6. **Empty-group semantics (grouped Min/Max):** empty group → output
   is `NA_REAL` (matches current R fallback behaviour via `matrixStats`
   on empty input).

## 4. New C++ kernels

All three follow repo conventions: `.h` headers when needed (none
here), `[[cpp11::register]]` entry points suffixed `_cpp`,
`openmp_shim.h` helpers (`DAFR_PARALLEL_FOR`), threshold-gated
parallel-for, cpp11 (not Rcpp).

### 4.1. `src/kernel_quantile_dense.cpp`

```cpp
[[cpp11::register]]
cpp11::writable::doubles kernel_quantile_dense_cpp(
    SEXP mat,               // INTSXP or REALSXP, column-major
    int axis,               // 0 = per-row (ReduceToColumn), 1 = per-col (ReduceToRow)
    double q,               // in [0, 1]
    int threshold);         // from .dafr_kernel_threshold()
```

- Type-7 quantile formula, matching `stats::quantile(x, q, type=7)`:
  `h = q*(n-1); lo=floor(h); hi=ceil(h); frac=h-lo;
  out = (1-frac)*v[lo] + frac*v[hi]`.
- Int branch (`TYPEOF(mat) == INTSXP`): `INTEGER_RO(mat)` + cast to
  double at read time. Double branch: `REAL_RO(mat)`.
- Algorithm: per column (axis=1) or per row (axis=0), copy values into
  a thread-local `std::vector<double>` scratch buffer, `std::nth_element`
  twice (for `lo` and `hi` ranks), interpolate. No implicit-zero split
  (unlike CSC kernel) — dense has no implicit zeros.
- Parallelism: `DAFR_PARALLEL_FOR(ncol >= threshold)` for axis=1,
  `DAFR_PARALLEL_FOR(nrow >= threshold)` for axis=0. Thread-local
  scratch — no contention.
- NA / NaN: any NA_INTEGER or NaN/NA_REAL in the column/row →
  output is NA_REAL for that column/row (short-circuit at the
  value-scan step).
- Empty (n=0): NA_REAL. Single value: that value.

### 4.2. `src/kernel_mode_dense.cpp`

```cpp
[[cpp11::register]]
cpp11::writable::doubles kernel_mode_dense_cpp(
    SEXP mat,
    int axis,
    int threshold);
```

- Tiebreak: first-encountered wins on equal counts — matches
  `.op_mode`: `ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]`.
  For a column (axis=1), scan rows 0..n-1; for row (axis=0), scan
  columns 0..n-1. First value to reach the winning count wins.
- Type branch as in 4.1. For Int input, convert to double at hash-key
  time (bit-exact — any int32 value has a unique double representation).
- Algorithm: `std::unordered_map<double, int>` counts +
  `std::unordered_map<double, int>` `first_seen` index. Final pass
  picks the (count, first_seen) maximum with first-seen as tiebreak.
  No implicit-zero bookkeeping (`first_zero_row` trick from CSC kernel
  is dropped).
- NaN: NaN bucketed in its own hash slot (`std::hash<double>` treats
  NaN payload bits as the hash input; since `std::unordered_map`
  compares keys with `==` which is always false for NaN, each NaN ends
  up in a separate bucket — **this is a divergence we must handle
  explicitly** — use `std::isnan(v)` to route all NaNs to a single
  sentinel counter). Matches R's behaviour where `unique(c(NaN, NaN))`
  returns a single NaN.
- NA_REAL (distinguished from NaN by payload via `ISNA`): bucketed
  separately from NaN; can win as the mode if most frequent.
- Parallelism: `DAFR_PARALLEL_FOR` per axis, thread-local hashmap.

### 4.3. `src/kernel_grouped_minmax_dense.cpp`

```cpp
[[cpp11::register]]
cpp11::writable::doubles_matrix<cpp11::by_column> kernel_grouped_minmax_dense_cpp(
    SEXP mat,
    cpp11::integers groups,
    int ngroups,
    int axis,               // 2 = G2 (ngroups × ncol), 3 = G3 (nrow × ngroups)
    int variant);           // 0 = Min, 1 = Max
```

- Single-threaded (matches `kernel_grouped_rowsum_dense_cpp`).
  Per-output-cell accumulator with per-group init sentinel makes
  OpenMP-across-groups racey on the output matrix; keeping serial
  is simplest and the 9b rowsum kernel established that the
  683-cell fixture is not CPU-bound here.
- Accumulator init: per `(i, g)` cell, `+∞` (Min) or `-∞` (Max).
  First non-NA observation replaces the sentinel;
  `std::min` / `std::max` thereafter.
- NA propagation: once a `(i, g)` cell sees NA, it stays NA
  (bool-vector tracking, same pattern as
  `kernel_grouped_rowsum_dense.cpp:63`).
- Empty group: the `±∞` sentinel remains — post-process in R
  dispatch layer to convert to `NA_REAL` (simpler than tracking
  "group-seen" in C++).
- Int type branch as in 4.1/4.2.

## 5. R dispatch changes

Four edits in `R/query_eval.R`; no handwritten `cpp11.R` changes
(auto-generated).

### 5.1. Quantile / Mode dense branches

**`R/query_eval.R:671–688`** (ReduceToColumn, per-row, axis=0):

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

**`R/query_eval.R:773–789`** (ReduceToRow, per-col, axis=1):
symmetric; replace `matrixStats::colQuantiles` /
`apply(m, 2, op_mode_fn)` with the new kernels at `axis = 1L`.

Median dispatch (`matrixStats::colMedians`, `matrixStats::rowMedians`)
is untouched.

### 5.2. Grouped Min/Max dispatch

**`R/query_eval.R:1104–1122`** (G2, `.grouped_dense_rowsum`
`axis == 2L` branch):

```r
switch(label,
    Max = {
        out <- kernel_grouped_minmax_dense_cpp(
            m, groups = gi, ngroups = ngroups,
            axis = 2L, variant = 1L)
        # Post-process: empty groups (-Inf sentinel for Max) -> NA_REAL
        .minmax_empty_to_na(out, gi, ngroups, axis = 2L)
    },
    Min = {
        out <- kernel_grouped_minmax_dense_cpp(
            m, groups = gi, ngroups = ngroups,
            axis = 2L, variant = 0L)
        # Post-process: empty groups (+Inf sentinel for Min) -> NA_REAL
        .minmax_empty_to_na(out, gi, ngroups, axis = 2L)
    },
    # Fallback for unknown ops (GeoMean, etc.) — unchanged from 9b.
    kernel_grouped_reduce_dense_cpp(
        m, group = gi, ngroups = ngroups,
        n_in_group = n_in_group, axis = axis,
        op = label, eps = eps,
        threshold = .dafr_kernel_threshold())
)
```

**`R/query_eval.R:1152–1170`** (G3, `axis == 3L` branch):
symmetric; replace `matrixStats::rowMaxs`/`rowMins` loops with
`kernel_grouped_minmax_dense_cpp(..., axis = 3L, ...)`.

A small helper `.minmax_empty_to_na(out, gi, ngroups, variant)` lives
in the same file and scrubs the `±Inf` sentinels from empty groups.
Tabulate `gi`, find groups with zero count, zero the corresponding
output row (axis=2) or column (axis=3) to `NA_REAL`. One-liner
using `tabulate(gi, ngroups) == 0L`.

### 5.3. What does NOT change

- `R/operations.R` `.op_quantile` / `.op_mode` stay source of truth
  for formula authority.
- User-visible R API: unchanged. No new options, no new exports, no
  new public kernel surface.
- CSC paths: untouched.
- `NAMESPACE`, `DESCRIPTION`, `Makevars`: no edits.

## 6. NA / NaN / edge case contract (per-kernel)

### Quantile

| Input condition | Output |
|---|---|
| All values non-NA, non-NaN | Type-7 interpolated quantile |
| Any NA_INTEGER in column | `NA_REAL` |
| Any NaN in column | `NA_REAL` |
| Any NA_REAL in column | `NA_REAL` |
| Empty column (n=0) | `NA_REAL` |
| Single value | that value |
| All equal | that value |

Note: this is stricter than `stats::quantile` (which returns NaN
mid-sort when NaN is present). The strictness matches
`matrixStats::colQuantiles` default `na.rm=FALSE` behaviour.

### Mode

| Input condition | Output |
|---|---|
| Well-defined mode | mode value |
| Count tie | first-encountered value (scan order = row index for axis=1, col index for axis=0) |
| Any NaN in column | NaN bucketed; can win as mode |
| Any NA_REAL | NA_REAL bucketed separately; can win as mode |
| Empty column | `NA_REAL` (matches `.op_mode(numeric(0))` which returns NA_real_) |
| All equal | that value |

Divergence note: Mode is bit-exact with `.op_mode` (integer counts
+ first-seen tiebreak), *not* tolerance-based.

### Grouped Min/Max

| Input condition | Output(i, g) |
|---|---|
| Non-NA group-g values for row i | min / max over those values |
| Any NA value in group-g row-i | `NA_REAL` ("once NA, always NA") |
| Empty group g (no j with gi[j] == g) | `NA_REAL` (post-process strips ±∞) |
| Single-element group | that element |

## 7. Tests

Three new `test-kernel-*.R` files. Target: +30–40 assertions total.

### 7.1. `tests/testthat/test-kernel-dense-quantile.R`

Fixtures: 5×10 and 100×50 matrices, both Int32 and double. Quantiles
swept `q ∈ {0, 0.25, 0.5, 0.75, 1.0}`.

Cases:
1. Double axis=1 bit-match vs `matrixStats::colQuantiles(..., type=7)`
   within `sqrt(.Machine$double.eps)`.
2. Double axis=0 bit-match vs `matrixStats::rowQuantiles(..., type=7)`.
3. Int-aware parity: `kernel(int_mat, q)` ≈ `kernel(as.double(int_mat), q)`
   within tol.
4. Empty column → `NA_REAL`.
5. Single-value column → that value at any q.
6. NaN in column → `NA_REAL` (§6 contract).
7. NA_INTEGER in Int32 column → `NA_REAL`.
8. Parallelism invariance: `threshold = 1L` ≡ `threshold = .Machine$integer.max`.
9. Threshold gate: with `ncol < threshold`, serial path exercised
   (behavioural test via environment sanity, not a race-detection test).

### 7.2. `tests/testthat/test-kernel-dense-mode.R`

Cases:
1. Double axis=1 exact match vs `apply(m, 2, .op_mode)` (no tolerance).
2. Double axis=0 exact match vs `apply(m, 1, .op_mode)`.
3. Tiebreak: column `c(1, 2, 1, 2)` — both count 2; first-seen wins (value 1).
4. All-equal column → that value. All-zeros → 0.0.
5. Int-aware parity: `kernel(int_mat)` == `kernel(as.double(int_mat))`
   cell-for-cell.
6. NaN column: mode bucket merges all NaN instances (not separated by
   payload bit).
7. Empty column → `NA_REAL`.
8. Parallelism invariance.

### 7.3. `tests/testthat/test-kernel-grouped-minmax-dense.R`

Fixtures: 683×23 UMIs-shaped Int32 + 100-group vector; plus small
deterministic cases.

Cases:
1. G2 Max (axis=2): shape `(ngroups, ncol)`, values match the current
   R fallback on the same input.
2. G2 Min (axis=2).
3. G3 Max (axis=3): shape `(nrow, ngroups)`.
4. G3 Min (axis=3).
5. Int-aware parity vs double-promoted input (all four variants).
6. NA propagation: single NA in group-g row-i → output(i, g) = NA;
   other cells unaffected.
7. Empty group: output row (G2) or column (G3) for that group
   is all `NA_REAL` (post-process verified).
8. Single-element group → that element for every i (G2) or every j (G3).

### 7.4. Integration tests

No new integration tests required — existing Julia-queries golden
fixture at `tests/testthat/fixtures/julia-queries/` already covers
queries 026/028/043/047. Re-running the existing
`test-query-*.R` suite after the kernel changes is sufficient.

### 7.5. Expected post-slice test count

1840 → **1870–1880** passes, 0 fails, 1 skip, 1 warn (unchanged).

## 8. Bake-off validation

After kernels + tests + dispatch ship:

```
cd benchmarks && ./run.sh
```

**Pass condition:**
- `julia_queries_{026, 028, 043, 047}` all ≤ 1.5× (with light-tier
  threshold 2.0× as the hard line).
- All 75 other queries unchanged or better within run-to-run noise
  (≤ 10% variation).

**Fail condition:** any of the 4 above 2.0×, OR any previously-in-range
query slipping above its tier threshold.

Bake-off artifacts archived to
`dev/benchmarks/2026-04-22-post-slice-9c/`. `dev/benchmarks/perf-log.md`
and `benchmarks/perf-log.md` each get a "Slice 9c" entry: "4 queries
closed via dense Int-aware kernels. Final breach count 7 → 3."

## 9. File-by-file change list

**New (package repo):**
- `src/kernel_quantile_dense.cpp`
- `src/kernel_mode_dense.cpp`
- `src/kernel_grouped_minmax_dense.cpp`
- `tests/testthat/test-kernel-dense-quantile.R`
- `tests/testthat/test-kernel-dense-mode.R`
- `tests/testthat/test-kernel-grouped-minmax-dense.R`

**Regenerated (cpp11 auto):**
- `src/cpp11.cpp`
- `R/cpp11.R`

**Edited:**
- `R/query_eval.R` — four dispatch sites (see §5).
- `NEWS.md` — Slice 9c bullet.

**Unchanged (confirm no edit):**
- `R/operations.R` (formula authority).
- `DESCRIPTION`, `NAMESPACE`, `Makevars`, `Makevars.win`.
- All other `.cpp` kernels.

**Dev repo:**
- `dev/notes/2026-04-22-slice-9c-design.md` — this document.
- `dev/plans/2026-04-22-slice-9c-perf-closure.md` — to be written by
  writing-plans skill.
- `dev/benchmarks/perf-log.md` — appended at exit.
- `dev/benchmarks/2026-04-22-post-slice-9c/` — bake-off output.

## 10. Risks and mines

- **Int32 cast to double** in the Mode hashmap: an Int32 value casts
  to a bit-exact double (Int32 is representable exactly in double's 53-bit
  mantissa). No precision loss; hashing the double works. Tested by
  case 7.2.5.
- **`std::unordered_map<double, int>` NaN bucketing** — std library
  has undefined behavior for NaN keys because `NaN != NaN`. Mitigated
  by routing NaN values through a dedicated sentinel counter in the
  kernel. Flagged in §4.2.
- **Threshold bypass by `Inf`** — documented mine from 9b.
  `dafr.kernel_threshold = Inf` puts the dispatch into R fallback, not
  a user-visible path. Tests use finite thresholds only.
- **`matrixStats::colMaxs` on single-row matrix warning** — the
  existing R fallback uses this; the new kernel path skips it entirely
  so any latent warning-noise there is gone. Worth a note.
- **Formula authority enforcement:** kernels must bit-match
  `.op_quantile` / `.op_mode` / group-fallback R code. Regression net
  (1840 tests) + new tests (7.1/7.2/7.3) catch drift.
- **Empty-group handling correctness:** the `.minmax_empty_to_na`
  helper must run on both axis=2 and axis=3 paths. One integration
  test per axis (cases 7.3.7) verifies.
- **9b G3 memory concern not touched:** this slice doesn't revisit
  `kernel_grouped_reduce_csc_cpp` axis=3. The grouped-minmax-dense
  kernel is single-threaded, so it doesn't reintroduce the concern.
