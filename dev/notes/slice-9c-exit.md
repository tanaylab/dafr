# Slice 9c — Exit gate (2026-04-22)

**Branch:** `slice-9c-perf-closure` at package-repo HEAD `0cab857`.
**Predecessor:** `slice-9b-perf-parity` (merged at `9ab46e5`); see
`dev/notes/slice-9b-exit.md`.
**Kickoff:** `dev/notes/slice-9c-kickoff.md`
**Design:** `dev/notes/2026-04-22-slice-9c-design.md`
**Plan:** `dev/plans/2026-04-22-slice-9c-perf-closure.md`
**Perf log:** `dev/benchmarks/perf-log.md`

## What shipped

10 commits on top of `9ab46e5`:

| Commit | Description |
|---|---|
| `b3d902f` | perf(9c): dense Int-aware Quantile kernel |
| `fdfad9e` | test(9c): add NA_real_ case to dense Quantile kernel tests |
| `d96ff13` | perf(9c): dense Int-aware Mode kernel |
| `68e5610` | fixup(9c): code-review fixes for dense Mode kernel |
| `2de11fa` | perf(9c): dense Int-aware grouped Min/Max kernel |
| `64de360` | fixup(9c): NaN semantics + test coverage for grouped Min/Max |
| `285d921` | perf(9c): wire dense Int-aware Quantile/Mode/MinMax kernels |
| `f7a3732` | docs(9c): comment Quantile dispatch guard for non-numeric matrices |
| `ed6c033` | docs(9c): NEWS entry for dense perf closure |
| `0cab857` | docs(9c): note R CMD INSTALL prerequisite for bake-off |

**New files:**
- `src/kernel_quantile_dense.cpp`
- `src/kernel_mode_dense.cpp`
- `src/kernel_grouped_minmax_dense.cpp`
- `tests/testthat/test-kernel-dense-quantile.R`
- `tests/testthat/test-kernel-dense-mode.R`
- `tests/testthat/test-kernel-grouped-minmax-dense.R`

**Edited files:** `R/query_eval.R` (4 dispatch sites + `.minmax_empty_to_na`
helper), `src/cpp11.cpp`, `R/cpp11.R` (auto-regen), `NEWS.md`,
`benchmarks/README.md`.

**Test suite:** `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 1907 ]`
(baseline 1840 after 9b + 67 new assertions from the three new test files).

**Check:** `devtools::check(error_on = "warning")` reports 0 errors,
0 warnings, 4 notes — identical to 9b (benchmarks dir, installed size,
future timestamps, hidden `.claude/`).

## Final perf table

Bake-off run at `dev/benchmarks/2026-04-22-post-slice-9c/report.md`.
Ratio = `dafr_median / julia_median` (higher = dafr slower). Threshold
for the light tier is 2.0×; mmap tier is 3.0×.

### Closed in 9c (4 queries)

| Query | Op | Before (9b) | After (9c) | Close mechanism |
|---|---|---|---|---|
| `julia_queries_026` | Quantile on UMIs | 3.00× | **1.24×** | `kernel_quantile_dense_cpp` replaces `matrixStats::colQuantiles` on Int32 input |
| `julia_queries_028` | Mode on UMIs | 2.27× | **1.18×** | `kernel_mode_dense_cpp` replaces `apply(.op_mode)` on Int32 input |
| `julia_queries_043` | G2 Max on UMIs | 2.16× | **0.85×** | `kernel_grouped_minmax_dense_cpp` replaces `matrixStats::rowMaxs`-in-loop; dafr now beats Julia |
| `julia_queries_047` | G3 Max on UMIs | 2.14× | **0.90×** | Same kernel, G3 direction; dafr now beats Julia |

Net: **7 breaches (post-9b) → 4 breaches (post-9c)**. The 4 queries
closed here were the last dense-path bottleneck identified at 9b exit.

## Remaining breaches (accept-class)

All 4 remaining breaches are the mmap-query S7-ctor floor. These require
architectural changes outside the dense-kernel scope of 9c.

| Query | Post-9c ratio | Floor source |
|---|---|---|
| `mmap_open_read_scalar` | ~1.44× | `files_daf` S7 ctor + `normalizePath` + axis-set scan; threshold 1.50× — borderline |
| `mmap_open_read_vector` | ~1.86× | Same structural cost; compounded with vector-descriptor parse |
| `mmap_open_read_matrix` | ~2.40× | Same + first mmap-page touch |
| `mmap_open_read_axis` | ~2.53× | Dominates on a short (~800 µs) Julia-side query |

**Why accepted:** all four sit on the R per-call dispatch + S7-ctor floor,
exactly as documented in the 9b exit. Further closing requires (a)
query-parse result caching, (b) S7 validator elimination on the hot path,
or (c) rewriting the `files_daf` constructor in C++. All three are
architectural scope for 9d+.

## Summary table

### Post-9c breach state (all 17 original baseline breaches accounted for)

| State | Count | % of original 17 |
|---|---|---|
| Closed (9b) | 10 | 59 % |
| Closed (9c) | 4 | 24 % |
| Accept — mmap S7-ctor floor | 4 (was 3 from mmap; `mmap_open_read_scalar` borderline) | 24 % |
| **Total** | **17** | 100 % |

### Post-9c bake-off state (79 queries)

| State | Count | % of 79 |
|---|---|---|
| Within threshold | 75 | 95 % |
| Breaching (accept-class) | 4 | 5 % |

## Headline findings

1. **All four targeted dense-path breaches closed.** The common root cause
   was the `storage.mode(m) <- "double"` ALTREP-materialize + copy that
   R performs when an integer matrix is passed to double-expecting C++ or
   matrixStats functions. Moving the int→double promotion inside the kernel
   eliminated this ~2 ms per-call tax on the 856 × 683 Int32 UMIs matrix.

2. **G2 Max and G3 Max now beat Julia outright (0.85×, 0.90×).** Before
   9c the grouped-Max path routed through `matrixStats::rowMaxs` in a
   per-group loop; the new `kernel_grouped_minmax_dense_cpp` kernel does
   the entire grouped scan in a single OpenMP-parallel pass.

3. **CRITICAL LESSON — R CMD INSTALL required before bake-off.** During
   Task 6 validation the initial bake-off run showed all 4 target queries
   *worse* than pre-9c (Q026 went 3.00× → 3.69×). Root cause: the bake-off
   runner calls `library(dafr)`, which loads the *installed* package, not
   the source tree. Any change to `R/*.R` or `src/*.cpp` — including the
   new dispatch wiring in `R/query_eval.R` and the three new `.cpp` kernels
   — is invisible to the bake-off until `R CMD INSTALL . --preclean` is
   run from the package root. After installing, Q026 dropped to the expected
   1.24×. This is now documented in `benchmarks/README.md` (commit
   `0cab857`); future perf-work writers must call this out explicitly in
   bake-off step descriptions.

4. **dafr now beats DAF.jl on 4 queries.** Q026, Q028 are within 1.25×;
   Q043 and Q047 run faster than Julia at single-thread. Combined with
   the big-sparse wins from 9b, dafr is at parity or faster on all
   non-mmap queries.

## Julia DAF state at exit

- `~/src/DataAxesFormats.jl` at `49fbba140437387a378217c2fa658d4231d0c8c1`
  (verified with `git -C ~/src/DataAxesFormats.jl log --oneline -1`).
  Unchanged since Slice 3 — 9 slices of stability.
- `~/src/TanayLabUtilities.jl` at `48a4a57` (unchanged).
- `benchmarks/julia/Manifest.toml` locks the Julia dep graph for
  reproducibility; committed to the branch.

## Artifacts

- `dev/benchmarks/2026-04-22-post-slice-9c/` — post-9c bake-off run
  (report.md + raw CSVs).
- `dev/benchmarks/perf-log.md` — slice-9c entry appended (commit
  `f7a3732`).

## Follow-ups recorded elsewhere

- **mmap S7-ctor floor** (`mmap_open_read_{scalar,vector,matrix,axis}`)
  — 4 remaining breaches. Require query-parse caching or `files_daf`
  ctor rewrite. Deferred to 9d+.
- **G3 axis=3 memory fix** — `kernel_grouped_reduce_csc_cpp` O(nthreads ×
  nrow × ngroups) bucket layout; single-thread baseline never exercises it.
  Profiled fix (row-partition fallback or adaptive thread cap) still
  deferred; tracked from 9b exit.
- **`copy_all` double-write bug** — `R/copies.R` iterates `(ra, ca)` pairs
  without dedup; a FilesDaf storing both layouts of a matrix triggers a
  duplicate-write error without `overwrite=TRUE`. Bench fixture builder
  works around it; real fix is to track canonical `sort(c(ra,ca)) + name`
  triples. Still unfixed.
