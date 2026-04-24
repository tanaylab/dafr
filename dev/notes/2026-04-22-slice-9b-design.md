# Slice 9b — Perf parity with DAF.jl (design)

**Date:** 2026-04-22
**Predecessor:** Slice 9a (tag `slice-9a`, merge `358fe78`)
**Kickoff:** `dev/notes/slice-9b-kickoff.md`
**Scope directive:** "make sure we are as fast as the julia code" (user, 2026-04-22)
**Scope shape:** full Workstream B (harness + baseline + per-gap closes + G3 memory fix + exit).

## Decisions locked at design time

1. **Perf parity target shape:** tiered (kickoff recommendation).
2. **CI `assignInNamespace` + Windows path:** resolved pre-design at commit
   `50fa293` (test-helper namespace binding + `.is_absolute_path()`). CI green
   on all three platforms. Not a 9b task anymore.
3. **G3 kernel memory fix strategy:** deferred to post-baseline (T3), per
   kickoff decision #6. Locked after profile data lands.

## Tier gates

| Tier | Threshold (`dafr / julia`) | Covers |
|------|---|---|
| Kernel-bound | ≤ 1.2× | column/row reductions on sparse, grouped reductions |
| BLAS-backed dense | ≤ 1.1× | dense matrix ops that bottom out in BLAS on both sides |
| FilesDaf mmap I/O | ≤ 1.5× | open + read full matrix / vector |
| Dispatch-heavy light queries | ≤ 2.0× | trivial queries dominated by S7 + AST walk |

`ratio = dafr_median / julia_median`. `ratio > threshold` is a **breach**.

## Harness architecture

Two coordinated runners, one shared fixture, one comparison step.

### Directory layout

```
benchmarks/                                   (package repo, shipped with source)
    fixture/
        build-fixture.R                       # generates the shared FilesDaf corpus
        data/                                 # generated fixtures (gitignored)
    R/
        run-bakeoff.R                         # R-side runner (bench::mark)
    julia/
        run_bakeoff.jl                        # Julia-side runner (BenchmarkTools.jl)
        Project.toml                          # dev-deps DataAxesFormats, TanayLabUtilities
    queries.yaml                              # single source of truth for the query set
    compare.R                                 # joins both CSVs → ratio report + markdown
    README.md                                 # end-to-end reproduction steps
dev/benchmarks/                               (dev repo, tracks historical runs)
    YYYY-MM-DD-<label>/
        r-times.csv
        julia-times.csv
        comparison.csv
        report.md
    perf-log.md                               # append-only: commit × breach × before/after
```

### Fixture corpus

Both runners point at the same on-disk `FilesDaf` directories. Five fixtures
total, built by `build-fixture.R`:

1. **cells_daf** — literal `example_cells_daf()` written to FilesDaf. Hosts
   the 51 julia-queries records verbatim.
2. **big_sparse** — 10 000 × 10 000 `dgCMatrix`, 5 % nnz, reproducible seed
   (`set.seed(9b_001)`). Plus `group_100` vector (100 groups) and
   `group_1000` vector (1 000 groups) on rows for grouped ops.
3. **chain_triple** — three-level FilesDaf chain (base → mid → leaf) via
   `complete_chain`, for chain-reader dispatch measurement.
4. **view_renamed** — renamed-axis view stored via
   `complete_chain(..., axes = list(list("renamed_cell", "@ cell")))`.
5. **mmap_reopen** — same content as `cells_daf` but every benchmark iteration
   reopens it from disk (for I/O-dominated measurement, separated from
   already-open dispatch cost).

Fixture SHA256 computed after build and logged into the header of both
runner CSVs. `compare.R` refuses to join CSVs whose fixture checksums
disagree.

### Runners

**R runner** (`benchmarks/R/run-bakeoff.R`):

- Reads `queries.yaml`, filters by `--only <id>,<id>,...` / `--fixture <name>`
  if given.
- Opens each required fixture once.
- For each query: `bench::mark(expression, min_iterations = 5, filter_gc = FALSE)`.
- Emits `r-times.csv` with schema `query_id, query_text, category, fixture,
  median_time_ns, min_time_ns, gc_time_ns, allocations, n_iter`.
- Header lines (prefixed `#`) record: dafr commit, R version, platform,
  `OMP_NUM_THREADS`, BLAS backend, fixture SHA256.

**Julia runner** (`benchmarks/julia/run_bakeoff.jl`):

- Same `queries.yaml`, same `--only` / `--fixture` flags (julia uses
  `ArgParse.jl`, kept minimal).
- Each query: `@benchmark <expr> samples=100 seconds=10 evals=1` (or
  `BenchmarkTools.jl` defaults with the same iteration lower bound as R).
- Emits `julia-times.csv` with identical schema (`allocations` is bytes
  allocated from BenchmarkTools' `memory` field; column name `allocations`
  keeps the join trivial).
- Same header lines, just Julia-side versions.

**Comparison** (`benchmarks/compare.R`):

- Joins on `query_id`.
- Refuses to join if fixture SHA256 headers disagree.
- Computes `ratio = dafr_median / julia_median`.
- Tags each row with its tier and threshold, flags breach.
- Emits `comparison.csv` and `report.md` (markdown table sorted by absolute
  gap size within each category; "BREACHED" heading at top).

### Reproducibility guardrails

Baseline runs are **single-threaded** on both sides:

- R: `Sys.setenv(OMP_NUM_THREADS = "1")`, `RhpcBLASctl::blas_set_num_threads(1)`
  (added to Suggests), `options(dafr.kernel_threshold = Inf)` to disable
  parallel kernel paths for the baseline.
- Julia: `JULIA_NUM_THREADS=1`, `LinearAlgebra.BLAS.set_num_threads(1)`.

Parallel variants are labeled runs (`YYYY-MM-DD-par-<nthreads>`); baseline
ratios are the ones gates measure against. G3 memory investigation is a
parallel run at 128 threads on the lab machine.

## Query set (`benchmarks/queries.yaml`)

~86 queries, with distribution:

| Tier | Count | Source |
|---|---|---|
| light | 51 | All julia-queries fixture records verbatim |
| kernel | ~12 | Sum/Mean/Var/Max/Median/Mode/GeoMean on `big_sparse`, row + column |
| blas | ~4 | Dense double matrix ops that bottom out in BLAS |
| grouped | ~9 | G1 / G2 / G3 on `big_sparse` × {group_100, group_1000} |
| mmap | ~4 | `open_daf` + full-matrix and full-vector read on `mmap_reopen` |
| chain | ~4 | 3-layer read-through on `chain_triple` |
| complete | ~2 | `complete_daf` reopen + one query through reconstructed `view_renamed` |

Each entry in `queries.yaml`:

```yaml
- id: julia_queries_001
  text: "/ cell / age"
  category: light
  fixture: cells_daf
```

`queries.yaml` is the single source of truth read by both runners. The 51
light entries are machine-generated from the julia-queries fixture JSON by
`benchmarks/build-queries.R` so they stay in sync; the script rewrites ONLY
the light section (preserving the hand-authored ~35 perf-specific
entries). Run it after any julia-queries fixture change.

## Triage and optimization loop

### Step 1 — Baseline (T3)

Build fixtures, run both runners sequentially, commit the baseline at
`dev/benchmarks/YYYY-MM-DD-baseline/`. Baseline is the reference; subsequent
runs compare back to it.

### Step 2 — Classify each breach

Each breach gets one of four dispositions, decided in triage and confirmed
with the user before any fix work starts:

| Disposition | Meaning | Action |
|---|---|---|
| **Fix** | Matchable (kernel-bound, BLAS, mmap read) or matchable-with-effort (CSC iteration, FilesDaf dispatch, grouped-vector kernels without a C++ path) per kickoff ranking | Open sub-task Tk |
| **Defer** | Clear path but out of 9b scope | Add to exit note's "still deferred" |
| **Accept** | Genuinely irreducible (e.g., R parse cost on 2-char query) | Document in exit note with measurement |
| **Investigate** | Root cause unclear | Scoped investigation sub-task before fix-vs-accept decision |

### Step 3 — Per-gap close loop

For each **Fix** breach, one commit (or small commit chain). After each close:

1. `run-bakeoff.R --only <id>` — confirm breach closed on the affected query.
2. Every 3–5 closes: full-suite re-run — catches regressions from a local
   optimization that blew up something else.
3. Append row to `dev/benchmarks/perf-log.md`: `date | commit | breach_id |
   before | after | notes`.

### Step 4 — G3 memory fix

Baseline + a parallel (128-thread) run on lab machine will reveal which G3
option to take. Three candidates (kickoff decision #6):

- **Row-partition fallback** — at large `nrow`, rows are partitioned across
  threads, each thread owns a contiguous row band; reduction sync at end.
  Maintains parallelism at scale.
- **Adaptive thread cap** — if `nthreads × nrow × ngroups × sizeof(Acc) >
  budget`, reduce `nthreads` for this call. Simpler; loses parallelism.
- **Sequential fallback** — at extreme size, drop to single-threaded.
  Simplest; worst parallelism.

Fix lives in `src/kernel_grouped_reduce_csc.cpp` axis=3 path (plus the
matching paths in `kernel_grouped_quantile_csc.cpp` and
`kernel_grouped_mode_csc.cpp`). All three kernels currently share the
thread-bucket structure (FIXME at the allocation site, documented at Slice-8
exit). Locking option after data is in.

## Exit criteria

To declare Slice 9b done, all of:

1. All tier thresholds met on queries classified **Fix**, OR remaining breaches
   classified **Accept** with measured rationale in the exit note, OR remaining
   **Fix** items deferred to a named future slice with written justification.
2. G3 memory explosion resolved — real fix in code, OR fallback chosen and
   measured working on the lab's 128-core × 10k nrow × 100 ngroups case.
3. Full test suite + `devtools::check(error_on = "warning")` clean locally.
4. CI green on ubuntu + macos + windows.
5. `benchmarks/` harness reproducible on a clean checkout (`run-bakeoff.R`
   from the CLI + `run_bakeoff.jl` + `compare.R` produce a report).
6. NEWS entry for 9b.
7. Exit note at `dev/notes/slice-9b-exit.md` with final perf table and
   disposition-per-breach list.

## Known mines and invariants

- **Formula authority:** `R/operations.R` `.op_*` is the source of truth for
  every op's formula and edge-case behaviour; any kernel rewrite for perf
  MUST match those results bit-exactly. Regression safety net = existing
  1813-test suite.
- **Julia fixture must stay stable:** `~/src/DataAxesFormats.jl` at
  `49fbba140437387a378217c2fa658d4231d0c8c1` (verified unchanged across
  Slices 3–9a, seven slices). Slice 9a's fixture regen script
  (`dev/scripts/regen-julia-queries-fixture.jl`) produces the 51-record
  fixture byte-identically. Do not silently regenerate against a moved HEAD.
- **No `--no-verify` / no amend / no force-push.** NEW commits only.
- **cpp11 only** for any C++ kernel changes (NOT Rcpp).
- **`dafr.kernel_threshold` option** (default 1024L) gates parallel kernel
  dispatch. Baseline disables parallelism via `Inf`; parallel runs keep
  default.
- **G3 kernel memory explosion** is the hottest deferred issue from Slice 8;
  do not land a perf claim without resolving it.

## Task shape (estimate)

Rough estimate per kickoff: 8–15 tasks, 5–8 days depending on how many
breaches classify as **Fix**. Concrete task slots:

- **T1** — CI stabilisation. **Done pre-design** (commit `50fa293`).
- **T2** — Harness scaffold: `build-fixture.R`, `run-bakeoff.R`, `run_bakeoff.jl`,
  `compare.R`, `queries.yaml` with light tier populated. No measurements yet.
- **T3** — Baseline run. Commit fixtures (outside repo), report, raw CSVs.
- **T4** — Triage: classify each breach; confirm dispositions with user.
- **T5...Tn-1** — Per-gap closes. One commit per gap. Cadence: every 3–5 closes
  trigger a full-suite re-run.
- **Tn-1 (parallel)** — G3 memory fix based on profile data.
- **Tn** — NEWS + exit note + tag + merge.

The T5...Tn-1 tail is open-ended; it converges when all **Fix** breaches are
closed or reclassified.
