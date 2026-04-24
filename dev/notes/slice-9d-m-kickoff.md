# Slice 9d-M — Kickoff breadcrumb

**Date:** 2026-04-22
**Scope locked at kickoff:** Option M from the Slice 9c kickoff menu
— the G3 kernel thread-bucket memory fix.
**Predecessor:** Slice 9c (tag `slice-9c` on `main` at merge commit
`8674f4f`), exit gate at `dev/notes/slice-9c-exit.md`.

## What changed between Slice 9c exit and now

Slice 9c (branch `slice-9c-perf-closure`, merged 2026-04-22) shipped
three Int-aware dense C++ kernels and wired them into
`R/query_eval.R`. Bake-off against DAF.jl: **7 → 4 breaches**. The
four queries closed were `julia_queries_{026, 028, 043, 047}`; Q043
and Q047 now beat Julia outright. Remaining four breaches are all
the mmap S7-ctor floor (`mmap_open_read_{scalar, vector, matrix, axis}`),
architectural for 9d+ and outside the scope of 9d-M.

New since 9c exit: nothing in the package repo. 9d-M starts from a
clean `main` at `8674f4f`.

## Current state (as of this writing)

- **Package repo**: `/home/aviezerl/src/dafr-native`, branch `main`
  at merge commit `8674f4f`, tag `slice-9c`. Clean working tree
  (only untracked `.claude/`). Pushed to `tanaylab/dafr` origin.
- **Dev repo** (nested): `/home/aviezerl/src/dafr-native/dev`, remote
  `aviezerl/dafr-native-notes`. `main` at `e077cde` (post-9c
  design + plan committed). 9c design at
  `notes/2026-04-22-slice-9c-design.md`, plan at
  `plans/2026-04-22-slice-9c-perf-closure.md`, exit at
  `notes/slice-9c-exit.md`, perf ledger at
  `benchmarks/perf-log.md` (slice-9c entry at top).
- **Test status** (local): `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 1909 ]`.
- **Check status** (local, `devtools::check(error_on = "warning")`):
  0 errors, 0 warnings, 4 notes (benchmarks dir, installed size,
  future timestamps, hidden `.claude/` — all carried from 9b/9c).
- **CI status**: push of `8674f4f` + tag `slice-9c` is live; check
  GitHub Actions on `tanaylab/dafr` for ubuntu/macos/windows +
  altrep-sanity green before starting 9d-M.
- **Public surface**: unchanged (same 110 exports as 9c).
- **Bake-off harness**: reproducible from a clean checkout via
  `benchmarks/README.md`. 79-query set, 5 fixtures, SHA256-verified.
  `R CMD INSTALL . --preclean` is a documented prerequisite since
  9c commit `0cab857`.

## Locked scope — Option M: G3 kernel memory fix

### The concern (from 9b kickoff decision #6, 9c kickoff item 4)

`kernel_grouped_reduce_csc_cpp` at
`src/kernel_grouped_reduce_csc.cpp:72-113` uses a thread-bucket
accumulator pattern for axis=3 (G3, col-group) to avoid write
contention on the output matrix:

```cpp
// FIXME: this is O(nthreads * nrow * ngroups) memory. For large nrow
// and ngroups (e.g. 1e6 * 100 cells * 8 threads ~= 10 GB of Acc),
// consider a row-partitioned fallback. Not implemented in this slice.
std::vector<std::vector<Acc>> tacc(nthreads,
    std::vector<Acc>((size_t)nrow * (size_t)ngroups));
```

At metacell scale (128 threads × 10k nrow × 100 groups × `sizeof(Acc)`
which is roughly 48 bytes on x86-64), this is **~6 GB** of
thread-local accumulator. At 1e6 × 100 × 128 threads × 48 B that
becomes **~600 GB** — completely blown out.

### Sister kernels with the same pattern

The same FIXME lives in two sibling grouped-CSC kernels, both also
untouched since earlier slices:

- `src/kernel_grouped_mode_csc.cpp:163` — Entry-pointer thread
  buckets, same dimension.
- `src/kernel_grouped_quantile_csc.cpp:115` — Same.

The dense variant (`src/kernel_grouped_reduce_dense.cpp:61-63`)
does **not** have this issue — it uses local accs of length `ngroups`
only because dense iteration already fixes the row dimension. So
the fix is scoped to the three CSC kernels above.

### Three candidate strategies (from 9b kickoff decision #6)

Lock one after profiling:

1. **Row-partition fallback.** Split the `nrow` dimension across
   threads instead of the `ncol` dimension. Each thread owns a
   disjoint row range and writes output directly (no merge step).
   Memory: `O(nrow × ngroups)` — the output matrix itself, no
   thread buckets. Trade-off: each thread must scan *all* columns
   to find contributions to its row range — may not parallelise
   well for sparse inputs where most `i`-values cluster in a few
   rows. Best-suited when `ncol >> nrow`.

2. **Adaptive thread cap.** Cap `nthreads` such that
   `nthreads × nrow × ngroups × sizeof(Acc) < fraction-of-RAM`.
   Introduce `.dafr_grouped_g3_memory_budget` option (default maybe
   256 MiB). Simplest fix; preserves current algorithm; degrades
   gracefully. Trade-off: user can silently lose parallelism on
   large workloads without knowing why.

3. **Sequential fallback.** If the memory budget would be exceeded
   at any thread count, drop to single-threaded. Even simpler than
   #2 but throws away all parallelism above the budget. Trade-off:
   predictable behaviour at the cost of hard throughput ceiling.

### Profiling prerequisite

**Lock the strategy only after a 128-thread lab-machine profile.**
The 9b bake-off was single-threaded (`OMP_NUM_THREADS=1`), so the
memory path is untested in the parallel regime. Fabricate a
stress fixture: CSC matrix with `nrow ≈ 10000`, `ngroups = 100`,
then `OMP_NUM_THREADS=128` and run the full bake-off. Measure
peak RSS (e.g., via `/usr/bin/time -v` or `getrusage`). If peak
RSS exceeds plausible, strategy #1 (row-partition) becomes
attractive; otherwise #2 or #3.

### Concrete scope

- **Three kernels edited in lockstep:** `kernel_grouped_reduce_csc.cpp`,
  `kernel_grouped_mode_csc.cpp`, `kernel_grouped_quantile_csc.cpp`.
  All three share the thread-bucket anti-pattern and must be fixed
  together (otherwise one unfixed kernel becomes the new cap).
- **New tests:** stress fixture at `tests/testthat/test-kernel-grouped-g3-memory.R`
  exercising `nrow ≈ 5000`, `ngroups = 50`, `threshold = 1L` (force
  parallel) and asserting peak memory stays under a ceiling (via
  `gc()` + `gcinfo(TRUE)` or `bench::bench_process_memory()` — the
  exact instrumentation is design-phase).
- **New config option** if strategy #2 is chosen:
  `dafr.grouped_g3_memory_budget` in `R/options.R`.
- **No public API change.**

### Done signal

- Single-thread: no change to existing tests or bake-off numbers.
- 128-thread: stress test completes without OOM on the lab machine
  at the target fixture shape.
- Full test suite: `FAIL=0 PASS ≥ 1909` (new memory-stress tests
  adding ~5-10 assertions).

## Decision points to lock at 9d-M kickoff

1. **Stress fixture shape for profiling.** Proposed: 10k × 10k
   CSC matrix, 100 groups, density 0.01 (matches metacell range).
   Lock at design.
2. **Which of the three strategies** (row-partition, adaptive
   thread cap, sequential fallback). Lock only after profile.
3. **Whether to ship a combination.** E.g., "row-partition with
   adaptive thread cap fallback" — can close both edge cases but
   adds complexity.
4. **Do we also re-check the 9b exit mine about accumulators**
   (`Acc` struct size could be trimmed if some fields aren't
   needed in all ops) — orthogonal but related.

## Carry-over items NOT in 9d-M scope

- **mmap S7-ctor floor** (4 accept-class breaches from 9c exit).
  Separate architectural work; 9d-P or later.
- **`copy_all` double-write bug** (`R/copies.R` iteration without
  dedup, worked around in `benchmarks/fixture/build-big-sparse.R`
  via `overwrite=TRUE`). Small focused fix; candidate for 9d-C.
- **`bestify` heuristic for `copy_vector` / `copy_matrix`.**
  Long-tail feature from earlier kickoffs.
- **`reconstruct_axis` with pre-existing target axis.**
- **H5df / AnnData / Zarr backends.**
- **Long-vector (>2³¹) ALTREP scenarios.**
- **UInt32 > 2³¹ read arm** (Slice-2 inherited).
- **Multi-writer filesystem locking on FilesDaf.**
- **`computation()` dual-/triple-contract forms.**
- **`@examples` for the ~25 skipped exports.**

## Known mines (for the 9d-M agent to brief on)

- **`OMP_NUM_THREADS=1` in bake-off.** The current
  `benchmarks/R/run-bakeoff.R` and `benchmarks/julia/run_bakeoff.jl`
  run single-threaded by design. The G3 memory concern is a
  PARALLEL-only issue; the regular bake-off will not exercise it
  and must not be changed to exercise it (single-threaded is the
  apples-to-apples measurement). Stress tests for 9d-M live in
  `tests/testthat/` not `benchmarks/`.
- **Lab-machine access required for 128-thread profile.** Do not
  guess at memory budgets from the developer workstation's thread
  count — the whole point of this fix is the parallel regime.
- **Bake-off `empty_cache` per iteration is load-bearing** — do
  not remove; flagged in every prior breadcrumb.
- **`R CMD INSTALL . --preclean` before any bake-off run.** Added
  to `benchmarks/README.md` in 9c commit `0cab857`; do not forget.
- **`dafr.kernel_threshold` option** — existing lever at
  1024L default, gates parallel dispatch. Do NOT reuse this name
  for the new memory-budget option; it controls compute parallelism,
  not memory. Use a distinct name like
  `dafr.grouped_g3_memory_budget`.
- **`.Rprofile` sets `options(error = recover)`** — any new stress
  script must `options(error = NULL)` at entry.
- **`sys.frame(1)$ofile` doesn't work under `Rscript`** — use the
  `commandArgs(trailingOnly=FALSE)` + `--file=` idiom when making
  new Rscript entry points.
- **Formula authority** — `R/operations.R` `.op_*` remains source
  of truth. Any new code path for the grouped reductions must
  produce bit-identical output on the 1909-test regression net.
- **cpp11 (NOT Rcpp)**, **`.h` headers (NOT `.hpp`)**, **OpenMP via
  `openmp_shim.h`** (`DAFR_PARALLEL_FOR`), never raw pragmas.
- **All three CSC kernels must be fixed in lockstep** — otherwise
  the unfixed one caps the ceiling.

## Repo conventions (reinforced through Slice 9c)

- **4-space R indent.** No tabs.
- **S7 multi-dispatch always uses `list(ClassA, ...)` signatures.**
- **`#' @include` directives are load-bearing** for S7 method
  registration.
- **`format_get_*` returns plain arrays without dimnames**; `get_*`
  adds names.
- **`sort(..., method = "radix")`** for all listing returns.
- **`.assert_name(x, "x")` / `.assert_flag(x, "x")`** at public API
  boundary.
- **`.DAFR_UNDEF` sentinel + `.is_undef`** for optional-default args.
- **`sQuote()`** around names in error messages.
- **`.dafr_builtin` attribute** on default-op functions.
- **`%||%`** lives in `R/utils.R:1`; do not redefine.
- **No emojis.** Never `--no-verify` / `--amend` / force-push. Always
  NEW commits.
- **Native C++ headers use `.h`, not `.hpp`.**
- **cpp11 (NOT Rcpp)** for all C++ bindings.
- **Kernel naming**: `kernel_<op>_<layout>.cpp` with
  `[[cpp11::register]]` entry points suffixed `_cpp`. OpenMP via
  `openmp_shim.h` helpers (`DAFR_PARALLEL_FOR`, `dafr_omp_get_*`),
  never raw pragmas.
- **`dafr.kernel_threshold` option** (default 1024L) gates parallel
  kernel dispatch; access via `.dafr_kernel_threshold()`. **Do NOT
  set to `Inf`** — that bypasses the C++ kernel to an R-fallback;
  not a user experience.
- **Bake-off empty_cache** — both runners invalidate cache per
  iteration; this is the only way cold-query timings are honest.
- **Formula authority**: `R/operations.R` `.op_*` functions are
  authoritative; kernels MUST match.
- **`R CMD INSTALL . --preclean` before every bake-off run.** The
  bake-off calls `library(dafr)`, not `devtools::load_all`; stale
  installs silently produce false perf numbers.

## Auto-memory carry-over

Durable feedback in `~/.claude/projects/-.../memory/MEMORY.md`:

- **Slice 4 P3 TDD divergence** — bundled commits with all-tests-
  green acceptable.
- **Native port motivation + perf parity goal** — "match DAF.jl
  speed" first-tier since Slice 8 exit (2026-04-22). With 9c closed,
  non-mmap parity is essentially achieved; 9d-M is about making
  the parallel-scale use case safe, not about chasing more
  single-thread closures.
- **Model selection — use Opus freely** — Opus for design-heavy
  implementation, final whole-branch reviews, or speed-sensitive
  work.
- **L2 upstream PR declined** — do NOT re-raise at slice exits.
- **Bake-off must invalidate cache per iteration** — see mines.

## Julia DAF state at Slice 9c exit

- `~/src/DataAxesFormats.jl` at
  `49fbba140437387a378217c2fa658d4231d0c8c1` (verified 2026-04-22;
  unchanged since Slice 3 — nine slices of stability now).
- `~/src/TanayLabUtilities.jl` at `48a4a57` (unchanged).
- Both registered as Julia `dev` packages in conda env `dafr-mcview`
  (Julia 1.12.5).
- `benchmarks/julia/Project.toml` + `Manifest.toml` lock the exact
  Julia dep graph for bake-off reproducibility.
- Fixture sets in the package repo:
  - `tests/testthat/fixtures/julia-queries/` — 51 records (unchanged
    since Slice 9a).
  - `tests/testthat/fixtures/julia-chains/` (Slice 4, unchanged).
  - `tests/testthat/fixtures/julia-adapter/` (Slice 5, unchanged).
  - `tests/testthat/fixtures/julia-copies/` (Slice 6, unchanged).

## Ready-to-paste prompt for Slice 9d-M

> Start implementing Slice 9d-M of the native-R `dafr` package.
>
> - Package repo: `~/src/dafr-native` on branch `main`, tag
>   `slice-9c` marks the Slice 9c merge commit (`8674f4f`).
> - Dev repo: `~/src/dafr-native/dev` — separate nested git repo,
>   remote `aviezerl/dafr-native-notes`.
> - Kickoff breadcrumb: `~/src/dafr-native/dev/notes/slice-9d-m-kickoff.md`
>   (this document).
> - Slice 9c exit note: `~/src/dafr-native/dev/notes/slice-9c-exit.md`.
> - Bake-off perf ledger: `~/src/dafr-native/dev/benchmarks/perf-log.md`.
>
> **Scope is locked:** G3 kernel thread-bucket memory fix. Affects
> three CSC kernels (`kernel_grouped_reduce_csc`,
> `kernel_grouped_mode_csc`, `kernel_grouped_quantile_csc`) that
> all share an `O(nthreads × nrow × ngroups)` memory pattern.
> Start with `superpowers:brainstorming` to lock the strategy
> (row-partition / adaptive thread cap / sequential fallback —
> ideally after a 128-thread lab-machine profile), then
> `superpowers:writing-plans`, then
> `superpowers:subagent-driven-development`.
>
> **Decision points to lock at 9d-M kickoff:**
> 1. Stress fixture shape for the 128-thread profile (proposed:
>    10k × 10k CSC, 100 groups, density 0.01).
> 2. Which of the three strategies.
> 3. Whether to ship a combination (e.g., row-partition + adaptive cap).
> 4. Memory-budget option name (proposed:
>    `dafr.grouped_g3_memory_budget`, NOT reusing
>    `dafr.kernel_threshold`).
>
> **Model selection:** Opus for design-heavy dispatches (strategy
> selection, profile interpretation, final whole-branch review).
> Sonnet for mechanical per-kernel edits and test writing.
>
> **Mines to brief the 9d-M agent on:** see the "Known mines"
> section. Headline items:
> - All three CSC kernels must be fixed in lockstep.
> - Profile-before-strategy: 128-thread lab-machine access required.
> - `OMP_NUM_THREADS=1` bake-off is unchanged — stress tests live
>   in `tests/testthat/`, not `benchmarks/`.
> - Bake-off `empty_cache` calls are load-bearing; don't remove.
> - `R CMD INSTALL . --preclean` before any bake-off run.
> - Formula authority: `R/operations.R` `.op_*` is source of truth.
> - `dafr.kernel_threshold` MUST NOT be set to `Inf` anywhere.
>
> **Julia DAF state at handoff:** `~/src/DataAxesFormats.jl` at
> `49fbba140437387a378217c2fa658d4231d0c8c1` (unchanged since
> Slice 3 — nine slices). `~/src/TanayLabUtilities.jl` at `48a4a57`.
> Before regenerating any fixture, verify DAF.jl has not moved.
