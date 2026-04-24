# Slice 9b — Kickoff breadcrumb

**Date:** 2026-04-22
**Predecessor:** Slice 9a (tag `slice-9a` on `main` at merge commit
`358fe78`), exit gate at `dev/notes/slice-9a-exit.md`.

## What changed between Slice 9a exit and now

Slice 9a (branch `slice-9a-correctness`, merged 2026-04-22) closed the
Julia-parity correctness gaps that were blocking meaningful perf
comparison. Breaking: `GroupRowsBy`/`GroupColumnsBy` + `ReduceToRow`/
`ReduceToColumn` pairings swapped to match DAF.jl. New: `>>` parser
alias for G1, Convert accepts Julia type names (`Float32`/`Float64`/
`Int32`/`Int64`/`Bool`), `Int64` via `bit64::integer64` with
dim/dimnames preserved. Julia-queries fixture extended with 23 new
records (51 total), byte-parity verified. Axis-rename view round-trip
test added (already-worked confirmed).

## Current state (as of this writing)

- **Package repo**: `/home/aviezerl/src/dafr-native/`, branch `main` at
  merge commit `358fe78`, tag `slice-9a`. Clean working tree. **Pushed
  to origin** (`tanaylab/dafr`) on 2026-04-22.
- **Dev repo** (nested, separate git repo, now `aviezerl/dafr-native-notes`
  on GitHub): `/home/aviezerl/src/dafr-native/dev/`. 9a design spec at
  `notes/2026-04-22-slice-9a-design.md`, plan at
  `plans/2026-04-22-slice-9a-correctness.md`, exit at
  `notes/slice-9a-exit.md`. Pushed.
- **Test status**: 1813 PASS / 0 FAIL / 1 SKIP (pre-existing S4
  dispatch skip) / 1 WARN (pre-existing scran SVD notice). Up from
  Slice 8's 1744 by 69 new tests across parse, convert, fixture,
  and complete-daf round-trip.
- **Check status (local)**: `devtools::check(error_on = "note")`
  exits 1 due to the same 2 structural notes as Slice 8 (benchmarks
  dir + installed size). Underlying `R CMD check` is clean of
  errors and warnings.
- **CI status**: Slice 8's CI failed on both ubuntu and windows with a
  persistent `assignInNamespace("as.matrix.Matrix", ...)` error in
  `test-helpers.R` + 7 downstream failures. This is a Matrix-package
  version-skew issue unrelated to anything Slice 8 or 9a did; it
  will also affect 9b. Local tests pass because the local Matrix
  version matches what the helper expects. **This is a mine — see
  the 9b mines list below.**
- **Public surface**: 110 exports, unchanged.

## Overall progress map

After Slice 9a, correctness parity with DAF.jl for all currently-tested
query surface is complete. What remains is performance parity (this
slice) plus the long tail (niche backends, `bestify`, etc.).

## Slice 9b scope — user directive

User directive at Slice 8 exit (2026-04-22): **"make sure we are as
fast as the julia code"**. Workstream B from the original Slice 9
kickoff is the entire scope of 9b.

### Goal

Establish a reproducible bake-off harness `dafr` ↔ DAF.jl, measure the
gap query-by-query, and close every gap where the path is plausibly
matchable (kernel-bound ops, BLAS-backed dense ops, FilesDaf mmap I/O,
CSC iteration). Accept irreducible dispatch overhead for light queries
with an honest "within 2×" target.

### Workstream B items (reproduced from Slice 9 kickoff)

1. **Build the bake-off harness** at `benchmarks/` (package repo) +
   `dev/benchmarks/` (CSV outputs). Runs identical queries on `dafr`
   (native R) and DAF.jl (Julia) against the same `FilesDaf` data.
   Uses `bench::mark` on the R side, `BenchmarkTools.jl` on the Julia
   side. Emits a CSV with `query | dafr_time | julia_time | ratio`.
   Must be idempotent, reproducible across machines.

   Note: **this is a new harness**, not an extension of the existing
   `dev/benchmarks/run-bakeoff.R` — that one compares our C++ kernels
   against a scratch RcppEigen package, not against DAF.jl.

2. **Query set for the bake-off.** Start with every query in the
   Slice-9a julia-queries fixture (51 records). Plus:
   - Heavy reductions on a 10k × 10k sparse `dgCMatrix` (after a
     `copy_all` of `example_cells_daf()`).
   - Grouped reductions on the same (G1, G2, G3 — now semantically
     aligned after 9a).
   - FilesDaf open + read a full matrix (I/O + mmap path).
   - Chain reader with 2–3 layers.
   - `complete_chain` + `complete_daf` round-trip.

3. **Identify gaps.** For every query where `ratio = julia_time /
   dafr_time < 0.7` (Julia is ≥1.4× faster), open a sub-task.
   Expected gap categories (ranked by matchability):

   - **Matchable:** kernel-bound ops (already competitive per Slice 8
     — verify), BLAS-backed dense ops, mmap read.
   - **Matchable with effort:** CSC iteration overhead, FilesDaf
     dispatch, grouped-vector kernels (no C++ fast path yet).
   - **Hard to match:** query parse + dispatch for light queries. R's
     S7 multi-dispatch + AST walk is inherently slower than Julia's
     JIT. Document irreducibility; aim for "within 2×" on short
     queries.

4. **Fix the G3 kernel memory explosion at scale.** From Slice 8
   deferred: `kernel_grouped_reduce_csc_cpp` axis=3 thread-bucket is
   O(nthreads × nrow × ngroups). At 128 threads × 10k nrow × 100
   ngroups = 6.7 GB, making the "fast" path slower than baseline and
   unusable on the lab's 128-core machines at metacell scale. See
   decision #6 below for options.

### Decision points for 9b kickoff (from original Slice 9 kickoff)

Still open, deferred from Slice 9a:

**#5. Perf parity realistic target.** Three options:
- **Blanket** — every query ≤ 1.0× Julia. Unrealistic for light
  queries where R's dispatch cost is intrinsic.
- **Hot-path** — kernels + I/O only. Pragmatic, but lets light-query
  overhead accumulate in user workflows.
- **Tiered** — specific gates per op category. E.g., kernels ≤ 1.2×,
  BLAS ops ≤ 1.1×, mmap I/O ≤ 1.5×, dispatch-heavy light queries ≤ 2×.
  Recommend tiered; it sets honest expectations without giving up on
  each tier.

**#6. G3 kernel memory fix strategy.** Three options; decide from
profile data:
- **Row-partition fallback** — at large `nrow`, partition rows across
  threads instead of duplicating the full bucket per thread. Each
  thread owns a contiguous row band; cross-band synchronization at
  reduction end. Complex but maintains parallelism.
- **Adaptive thread cap** — if `nthreads × nrow × ngroups ×
  sizeof(Acc) > memory_budget`, reduce `nthreads` for this call.
  Simpler. Loses parallelism at scale.
- **Sequential fallback** — at extreme sizes, fall back to
  single-threaded. Simplest. Worst parallelism.
  Pick based on what the bake-off at 128-core × 10k × 100 reveals.

## Known mines for Slice 9b

### From Slice 8 / carried through 9a

- **CI is broken on ubuntu + windows** with
  `assignInNamespace("as.matrix.Matrix", patched, ns = "Matrix")`:
  "no slot of name 'methods' for this object of class
  'derivedDefaultMethod'". 7 test failures cascade from this in
  `test-helpers.R` and `test-kernels-slice8.R`. Local tests pass.
  The helper assumes a Matrix-package internal that no longer exists
  in newer versions. **This needs a real fix before 9b exits**, since
  perf claims need CI validation to be credible. Candidate: update
  the `assert_no_densify_during` helper to use a different patching
  mechanism, or skip the tests on CI with a clear reason, or pin a
  Matrix version.

- `kernel_grouped_reduce_csc_cpp` axis=3 thread-bucket memory is
  `O(nthreads × nrow × ngroups)` (FIXME at the allocation site).
  Same concern in `kernel_grouped_quantile_csc_cpp` and
  `kernel_grouped_mode_csc_cpp` axis=3 paths.

- `Acc::push` in `src/kernel_grouped_acc.h` unconditionally computes
  `log(v + eps)` even for non-GeoMean ops (minor perf waste; ~5M
  wasted log calls on a 10k × 10k × 5% nnz input).

- `derive_op` silently returns 0 for unknown op strings (typo-safety
  concern — should error).

- `.op_convert` sparse→character still densifies (no sparse character
  class in R; accepted).

### From Slice 9a

- **Bool-on-matrix intrinsic divergence** with DAF.jl: Julia's
  `Bool(x)` is strict (`InexactError` on values > 1); R's `as.logical`
  is permissive. Fixture has no matrix Bool record. If the bake-off
  exercises matrix Bool convert, the output will differ — document or
  skip that specific query.

### Regen script reproducibility — now fixed

The Slice 9a T6 fixture was extended via a "one-shot" run that wasn't
reflected in the committed regen script, leaving them out of sync.
Fixed in dev/ commit `b501b28` (2026-04-22). Running
`dev/scripts/regen-julia-queries-fixture.jl` now reproduces the full
51-record fixture byte-identically. Do not re-introduce this gap in 9b
fixture work.

## Suggested 9b task shape

If Option I / one-shot execution (similar to 9a):

- **T1** — CI stabilisation: fix the `assignInNamespace` helper or
  skip-with-reason on CI. Needs to ship before any perf claim.
- **T2** — design + scaffold the bake-off harness (R driver + Julia
  driver + comparison CSV emitter). Commit with the 51-query baseline
  from the fixture.
- **T3** — run the harness, capture the first perf map. Commit the
  CSV + a `benchmarks/slice-9b-baseline-<date>.md` narrative.
- **T4** — classify gaps per decision #5's tiers, open one sub-task
  per gap that breaches its tier.
- **T5...** — close gaps, one per commit. Rerun the harness after each
  close, update the CSV.
- **Tn-1** — G3 kernel memory fix (decision #6).
- **Tn** — NEWS + exit + merge.

Realistic size estimate: 8–15 tasks, 5–8 days depending on how many
gaps breach their tier. The tail of "close one more gap" is naturally
stop-able — declare 9b done when all tier thresholds are met or the
remaining gaps are documented as accepted-divergence.

## Still deferred after Slice 9a (unchanged)

- `bestify` heuristic for `copy_vector` / `copy_matrix`.
- `reconstruct_axis` with a pre-existing target axis.
- H5df / AnnData / Zarr backends.
- Long-vector (>2³¹) ALTREP scenarios.
- UInt32 > 2³¹ read arm (Slice-2 inherited).
- Multi-writer filesystem locking on FilesDaf.
- `computation()` dual-/triple-contract forms.
- `@examples` for the ~25 skipped exports.

## Repo conventions (reinforced through Slice 9a)

- **4-space R indent.** No tabs.
- **S7 multi-dispatch always uses `list(ClassA, ...)` signatures.**
- **`#' @include` directives are load-bearing** for S7 method
  registration.
- **`format_get_*` returns plain arrays without dimnames**; `get_*`
  adds names.
- **`sort(..., method = "radix")`** for all listing returns.
- **`.assert_name(x, "x")` / `.assert_flag(x, "x")`** at public-API
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
  Manifest.toml).
- **Kernel naming**: `kernel_<op>_<layout>.cpp` with
  `[[cpp11::register]]` entry points suffixed `_cpp`. OpenMP via
  `openmp_shim.h` helpers (`DAFR_PARALLEL_FOR`, `dafr_omp_get_*`),
  never raw pragmas.
- **`dafr.kernel_threshold` option** (default 1024L) registered in
  `R/options.R` via `.dafr_default_options`; access via
  `.dafr_kernel_threshold()` which wraps `dafr_opt()`.
- **Formula authority**: `R/operations.R` `.op_*` functions are
  authoritative for every op's formula and edge-case behaviour.
  Kernels MUST match.

## Auto-memory carry-over

Durable feedback in `~/.claude/projects/-.../memory/MEMORY.md`:

- **Slice-4 P3 TDD divergence** — bundled-commit with all-tests-green
  acceptable; applies to 9b for any test-infrastructure rewrites
  (e.g. the `assignInNamespace` helper fix).
- **Native port motivation + perf parity goal** — "match DAF.jl speed"
  is now a first-tier goal as of 2026-04-22.
- **Model selection — use Opus freely** — Opus for design-heavy
  implementation (bake-off harness design, G3 kernel redesign) and
  final whole-branch reviews. Sonnet for mechanical per-op optimization
  passes and test rewrites.
- **L2 upstream PR declined** — do NOT re-raise at slice exits.

## Julia DAF state at Slice 9a exit

- `~/src/DataAxesFormats.jl` at
  `49fbba140437387a378217c2fa658d4231d0c8c1` (verified 2026-04-22 in
  T6; unchanged since Slice 3 — seven slices of stability now).
- `~/src/TanayLabUtilities.jl` at `48a4a57` (unchanged).
- Both registered as Julia `dev` packages in conda env `dafr-mcview`
  (Julia 1.12.5).
- Fixture sets in the package repo:
  - `tests/testthat/fixtures/julia-queries/` — **51 records**
    (extended in Slice 9a T6).
  - `tests/testthat/fixtures/julia-chains/` (Slice 4, unchanged).
  - `tests/testthat/fixtures/julia-adapter/` (Slice 5, unchanged).
  - `tests/testthat/fixtures/julia-copies/` (Slice 6, unchanged).

## Ready-to-paste prompt for Slice 9b

> Start implementing Slice 9b of the native-R `dafr` package.
>
> - Package repo: `~/src/dafr-native/` on branch `main`, tag `slice-9a`
>   marks the Slice 9a merge commit (`358fe78`).
> - Dev repo: `~/src/dafr-native/dev/` — separate nested git repo,
>   remote `aviezerl/dafr-native-notes`.
> - Kickoff breadcrumb: `~/src/dafr-native/dev/notes/slice-9b-kickoff.md`
>   (this document).
> - Slice 9a exit note: `~/src/dafr-native/dev/notes/slice-9a-exit.md`.
>
> **Scope directive (user, 2026-04-22):** "make sure we are as fast as
> the julia code". Workstream B from the original Slice 9 kickoff is
> the entire scope of 9b — bake-off harness, gap analysis, per-gap
> optimization commits, G3 kernel memory fix.
>
> **Decision points to lock at 9b kickoff:**
> 1. Perf parity target shape (blanket / hot-path / tiered — recommend
>    tiered).
> 2. G3 kernel memory fix strategy (row-partition / adaptive thread
>    cap / sequential fallback — decide from profile data).
> 3. Whether to fix the CI `assignInNamespace` issue as 9b T1 (load-
>    bearing for credible perf claims) or as an orthogonal fix that
>    ships first on its own.
>
> Use `superpowers:brainstorming` to lock scope + decision points,
> then `superpowers:writing-plans` to draft the slice plan, then
> `superpowers:subagent-driven-development` to execute.
>
> **Model selection:** Opus for design-heavy dispatches (bake-off
> harness design, G3 kernel redesign), final whole-branch review.
> Sonnet for mechanical per-op kernel tweaks, test rewrites, and
> benchmark result triage.
>
> **Mines to brief the Slice 9b agent:** see the "Known mines for
> Slice 9b" section. Headline items:
> - CI is broken (Matrix `assignInNamespace` — 7 failures). Fix before
>   perf claims land.
> - G3 kernel thread-bucket memory is the hottest deferred issue.
> - Formula authority: `R/operations.R` `.op_*` is source of truth.
> - Do NOT re-introduce the regen-script / fixture sync gap closed
>   in `b501b28`.
>
> **Julia DAF state at handoff:** `~/src/DataAxesFormats.jl` at
> `49fbba140437387a378217c2fa658d4231d0c8c1` (unchanged since Slice 3
> — seven slices). `~/src/TanayLabUtilities.jl` at `48a4a57`. Before
> regenerating any fixture, check whether DAF.jl has moved.
