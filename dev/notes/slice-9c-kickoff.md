# Slice 9c — Kickoff breadcrumb

**Date:** 2026-04-22
**Predecessor:** Slice 9b (tag `slice-9b` on `main` at merge commit
`9ab46e5`), exit gate at `dev/notes/slice-9b-exit.md`.

## What changed between Slice 9b exit and now

Slice 9b (branch `slice-9b-perf-parity`, merged 2026-04-22) landed the
reproducible `benchmarks/` bake-off harness and six perf fixes. Of the
17 original breaches against DAF.jl: 10 closed, 2 deferred (need new
C++ kernels), 5 accepted as R-dispatch floor. Headline finding:
**dafr beats DAF.jl on the `big_sparse` 10k × 10k corpus** across all
20 queries; remaining gaps are all on the tiny `cells_daf` fixture
where per-call dispatch and S7-ctor cost dominate.

New in 9b:
- `benchmarks/` — harness directory with fixture builders, both
  runners, comparison script, query set (YAML), and
  `Manifest.toml`-locked Julia deps.
- `src/kernel_grouped_rowsum_dense.cpp` — cpp11 kernel that promotes
  Int32 → double inline with optional sum-of-squares; replaces the
  prior `storage.mode(m) <- "double"` + `rowsum()` path for
  Sum/Mean/Var/Std/VarN/StdN on dense grouped reductions.
- `matrixStats::colVars`/`rowVars` fast path for dense non-grouped
  Var-family.
- BLAS-indicator-matrix shortcut (`m %*% ind`) for G3 Sum/Mean/Var.
- Regex fast-path for the three fixed-schema JSON files
  (`daf.json`, scalar descriptors, matrix/vector descriptors);
  `jsonlite::fromJSON` kept as fallback.
- `log()` in `Acc::push` gated to GeoMean-only (mine from Slice-8
  exit closed).

## Current state (as of this writing)

- **Package repo**: `/home/aviezerl/src/dafr-native/`, branch `main`
  at merge commit `9ab46e5`, tag `slice-9b`. Clean working tree.
  Pushed to origin (`tanaylab/dafr`) on 2026-04-22.
- **Dev repo** (nested): `/home/aviezerl/src/dafr-native/dev/`, remote
  `aviezerl/dafr-native-notes`. 9b design at
  `notes/2026-04-22-slice-9b-design.md`, plan at
  `plans/2026-04-22-slice-9b-perf-parity.md`, exit at
  `notes/slice-9b-exit.md`, triage at
  `notes/2026-04-22-slice-9b-triage.md`, perf ledger at
  `benchmarks/perf-log.md`. Pushed.
- **Test status**: `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 1840 ]`. Up
  from 1822 (post-CI-fix) by 18 new assertions for the
  grouped-rowsum kernel.
- **Check status (local)**: `devtools::check(error_on = "warning")`
  clean — 0 errors, 0 warnings, 4 notes (3 carried from Slice 8:
  benchmarks dir, installed size, future timestamps; plus the usual
  hidden `.claude/` note on local-only runs).
- **CI status**: ALL THREE PLATFORMS green on the 9b merge push
  (ubuntu, macos, windows R-CMD-check + altrep-sanity). No carried
  mines here.
- **Public surface**: unchanged (110 exports + `empty_cache` which
  was already exported).
- **Bake-off harness**: reproducible from a clean checkout via
  `benchmarks/README.md`. 79-query set, 5 fixtures, SHA256-verified.

## Deferred inventory — ordered by urgency / matchability

This is the scope menu for 9c. Pick one (or a bundle of two that
share a root cause).

### Perf closure — remaining breaches from 9b bake-off

1. **Dense Quantile and Mode C++ kernels** (closes 2 breaches:
   `julia_queries_026` Quantile at 3.00× and `_028` Mode at 2.27×).
   Both need cpp11 kernels analogous to `kernel_quantile_csc_cpp`
   and `kernel_mode_csc_cpp` but iterating over a dense column
   buffer. `matrixStats::colQuantiles` and `apply(+.op_mode)` are
   both measured at ~14–16 ms — not reducible without a new
   code path. **Concrete, well-scoped.**

2. **Max grouped in the new rowsum kernel** (closes 2 breaches:
   `julia_queries_043` G2 Max at 2.16× and `_047` G3 Max at 2.14×).
   Currently Min/Max fall through to the `matrixStats::rowMaxs` loop
   inside `.grouped_dense_rowsum()`. Adding a Max/Min accumulator to
   `kernel_grouped_rowsum_dense_cpp` would close these. Small, local
   change. **Low risk, small gain** — both queries are close to the
   2× light threshold and may flicker across runs.

3. **mmap/open structural floor** (4 breaches:
   `mmap_open_read_{matrix, vector, axis}` at 1.86–2.53× — _scalar_
   already closed in 9b). Residual is S7 ctor + `normalizePath` +
   axis-set scan on every `files_daf(..., mode="r")`. Two avenues:
   - (a) Query-parse result caching across reopens (weakens the
     "reopen" semantic but matches user reality).
   - (b) Rewrite `files_daf` constructor hot path in C++. Larger
     scope.
   **Non-trivial. May be 9d or later.**

### G3 kernel memory fix (Kickoff decision #6, now 2-slice-deferred)

4. **`kernel_grouped_reduce_csc_cpp` axis=3 thread-bucket memory
   explosion**. Deferred from Slice 8 exit, not exercised in 9b's
   single-thread baseline. At 128 threads × 10k nrow × 100 groups
   × 8 B = 6.7 GB of thread-local bucket. The hottest deferred
   mine. Three candidate strategies (kickoff decision #6):
   row-partition fallback, adaptive thread cap, sequential
   fallback. Should lock from a 128-core lab-machine profile
   (was Slice 9b Task 12.0, not run). Same concern in
   `kernel_grouped_quantile_csc_cpp` and
   `kernel_grouped_mode_csc_cpp` — fix in lockstep. **Real user
   impact under metacell-scale workloads.**

### Carry-over bugs

5. **`copy_all` double-write on relayout'd sources** (logged at
   9b Task 2 review). When a source FilesDaf stores a matrix in
   both `(A, B)` and `(B, A)` layouts (normal after `relayout=TRUE`),
   the default-option `copy_all` loop in `R/copies.R` iterates both
   axis-pair orderings and errors on the second write. Currently
   worked around in `benchmarks/fixture/build-big-sparse.R` with
   `overwrite=TRUE`; the real fix is to dedup `sort(c(ra, ca)) +
   name` triples in the outer loop. Small, focused. **Removes a
   fixture-builder workaround once shipped.**

### Long-tail features (from prior slice kickoffs)

6. `bestify` heuristic for `copy_vector` / `copy_matrix`.
7. `reconstruct_axis` with a pre-existing target axis.
8. H5df / AnnData / Zarr backends.
9. Long-vector (>2³¹) ALTREP scenarios.
10. UInt32 > 2³¹ read arm (Slice-2 inherited).
11. Multi-writer filesystem locking on FilesDaf.
12. `computation()` dual-/triple-contract forms.
13. `@examples` for the ~25 skipped exports.

## Recommended 9c scope shape

Depends on priority. Three natural bundles:

**Option P — Perf closure bundle.** Items 1 + 2. Closes 4 of the 7
remaining bake-off breaches with new C++ code. Clean, concrete, has
a clear "done" signal (breach goes from 7 → 3 or better). ~3–5 days,
5–8 tasks. Post-9c bake-off would show mostly "at R-dispatch floor"
as remaining gap — a clean exit story.

**Option M — G3 memory fix.** Item 4 alone. Requires lab-machine
access to profile at 128 threads, then implement one of three
strategies. Touches three grouped-CSC kernels in lockstep. ~2–4 days
if the profile data is clean; longer if row-partition turns out to be
the right answer (more complex C++). Most user-impactful single item.

**Option C — Cleanup bundle.** Items 2 + 5. Fix Max-in-kernel + the
`copy_all` bug. Small, self-contained, ships two items with real
test coverage. ~1–2 days, 3–5 tasks.

**Option X — Feature.** Any single item from 6–13. Each has its own
scope characteristics; `bestify` is the smallest of them (item 6),
H5df / AnnData are large (item 8).

Pick one bundle at kickoff, write the design + plan the same way as
9b.

## Known mines

- **Cache invalidation in `benchmarks/`** — both runners MUST call
  `empty_cache` / `empty_cache!` before each iteration, or every
  query hits a ~0.5ms cache-lookup ceiling (the 9b baseline bug).
  Don't remove these calls in any harness modification.
- **Formula authority** — `R/operations.R` `.op_*` remains source of
  truth. Any new C++ kernel (Quantile, Mode, Max-in-rowsum) must
  match bit-exactly via the existing 1840-test regression net.
- **`copy_all` double-write** — still present; `benchmarks/fixture/`
  still uses the `overwrite=TRUE` workaround until item 5 closes.
- **`.Rprofile` sets `options(error = recover)`** — benchmark
  scripts explicitly set `options(error = NULL)` early; any new
  Rscript entry points should do the same.
- **`sys.frame(1)$ofile` doesn't work under `Rscript`** — use the
  `commandArgs(trailingOnly=FALSE)` + `--file=` idiom (template in
  any `benchmarks/fixture/build-*.R`).
- **`storage.mode(m) <- "double"` on mmap ALTREP costs ~2 ms** for
  a 856 × 683 Int32 matrix (combination of ALTREP materialization
  + 4.6 MB alloc + copy). Avoid it on hot paths; use the Int-aware
  `kernel_grouped_rowsum_dense_cpp` if the op fits.
- **G3 kernel memory at 128 threads** — unfixed; item 4 above.

## Repo conventions (reinforced through Slice 9b)

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
- **Julia fixture scripts use an inline minimal JSON emitter** (no
  Manifest.toml). **Bake-off Julia side DOES use a Manifest.toml**
  (committed) because reproducibility across lab machines requires
  a pinned dep graph.
- **Kernel naming**: `kernel_<op>_<layout>.cpp` with
  `[[cpp11::register]]` entry points suffixed `_cpp`. OpenMP via
  `openmp_shim.h` helpers (`DAFR_PARALLEL_FOR`, `dafr_omp_get_*`),
  never raw pragmas.
- **`dafr.kernel_threshold` option** (default 1024L) gates parallel
  kernel dispatch; access via `.dafr_kernel_threshold()` which wraps
  `dafr_opt()`. **Do NOT set to `Inf`** — that bypasses the C++
  kernel to an R-matrixStats fallback; it's not what users see.
- **Bake-off empty_cache** — both runners invalidate cache per
  iteration; this is the only way cold-query timings are honest.
- **Formula authority**: `R/operations.R` `.op_*` functions are
  authoritative for every op's formula and edge-case behaviour.
  Kernels MUST match.

## Auto-memory carry-over

Durable feedback in `~/.claude/projects/-.../memory/MEMORY.md`:

- **Slice 4 P3 TDD divergence** — bundled commits with all-tests-
  green acceptable.
- **Native port motivation + perf parity goal** — "match DAF.jl
  speed" first-tier since Slice 8 exit (2026-04-22). Slice 9b
  delivered tiered parity; 9c perf items carry this further.
- **Model selection — use Opus freely** — Opus for design-heavy
  implementation, final whole-branch reviews, or speed-sensitive
  work.
- **L2 upstream PR declined** — do NOT re-raise at slice exits.
- **Bake-off must invalidate cache per iteration** — new as of 9b
  (2026-04-22); see mines list above.

## Julia DAF state at Slice 9b exit

- `~/src/DataAxesFormats.jl` at
  `49fbba140437387a378217c2fa658d4231d0c8c1` (verified 2026-04-22;
  unchanged since Slice 3 — eight slices of stability).
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

## Ready-to-paste prompt for Slice 9c

> Start implementing Slice 9c of the native-R `dafr` package.
>
> - Package repo: `~/src/dafr-native/` on branch `main`, tag
>   `slice-9b` marks the Slice 9b merge commit (`9ab46e5`).
> - Dev repo: `~/src/dafr-native/dev/` — separate nested git repo,
>   remote `aviezerl/dafr-native-notes`.
> - Kickoff breadcrumb: `~/src/dafr-native/dev/notes/slice-9c-kickoff.md`
>   (this document).
> - Slice 9b exit note: `~/src/dafr-native/dev/notes/slice-9b-exit.md`.
> - Bake-off perf ledger: `~/src/dafr-native/dev/benchmarks/perf-log.md`.
>
> **Scope to lock at kickoff:** pick one of the four bundles under
> "Recommended 9c scope shape" (P / M / C / X). Each is self-
> contained; start with `superpowers:brainstorming` to lock scope +
> decision points, then `superpowers:writing-plans` to draft the
> slice plan, then `superpowers:subagent-driven-development` to
> execute.
>
> **Decision points to lock at 9c kickoff:**
> 1. Which bundle (P / M / C / X)?
> 2. For Option M (G3 memory fix): which of the three strategies —
>    row-partition fallback, adaptive thread cap, sequential fallback?
>    Lock only after a 128-thread profile on the lab machine.
> 3. For Option P: does Max-in-kernel (item 2) ship in the same slice
>    as Quantile/Mode (item 1), or only item 1?
>
> **Model selection:** Opus for design-heavy dispatches (G3 fix
> strategy, new-kernel interface design, final whole-branch review).
> Sonnet for mechanical per-op implementation, test rewrites, and
> benchmark result triage. Haiku for small focused edits that the
> controller has already designed.
>
> **Mines to brief the Slice 9c agent:** see the "Known mines"
> section. Headline items:
> - Bake-off `empty_cache` calls are load-bearing; don't remove.
> - `copy_all` double-write is still live (item 5 scope).
> - G3 kernel memory is the hottest deferred item for parallel
>   workloads.
> - Formula authority: `R/operations.R` `.op_*` is source of truth.
> - `dafr.kernel_threshold` SHOULD NOT be set to `Inf` in any
>   benchmark or test — that's an R-fallback path, not a dafr user
>   experience.
>
> **Julia DAF state at handoff:** `~/src/DataAxesFormats.jl` at
> `49fbba140437387a378217c2fa658d4231d0c8c1` (unchanged since
> Slice 3 — eight slices). `~/src/TanayLabUtilities.jl` at `48a4a57`.
> Before regenerating any fixture, verify DAF.jl has not moved.
