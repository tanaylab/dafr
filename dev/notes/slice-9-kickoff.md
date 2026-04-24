# Slice 9 — Kickoff breadcrumb

**Date:** 2026-04-22.
**Predecessor:** Slice 8 (tag `slice-8` on `main` at merge commit
`f7978cc`), exit gate at `dev/notes/slice-8-exit.md`.

## What changed between Slice 7 exit and now

Slice 8 (branch `slice-8-matrix-fastpaths`, merged 2026-04-22) delivered
9 custom CSC/dense C++ kernels for all Slice-7 reductions (Var, Std,
VarN, StdN, Median, Quantile, GeoMean, Mode) plus shared grouped
reduction engines. Rewrote `.apply_reduction_grouped_*` to drop the
`vapply(..., numeric(1))` contract (enabling Mode-on-character via a
type-sniffing fallback). Preserved sparsity in `.op_convert` for
integer and logical types. Added `.matrix_type_ok` character + sparse-
integer recognition. Re-applied `base_daf_view` JSON on
`complete_daf` reopen. All 9 benchmark gates pass (ratios 2.17× to
86×).

## Current state (as of this writing)

- **Package repo**: `/home/aviezerl/src/dafr-native/`, branch `main`
  at merge commit `f7978cc`, tag `slice-8`. Clean working tree.
  **Not yet pushed to origin** — awaiting user confirmation.
- **Dev repo** (nested, gitignored by package repo):
  `/home/aviezerl/src/dafr-native/dev/`. Exit + kickoff notes in
  `notes/`, plan at `plans/2026-04-21-slice-8-matrix-fastpaths.md`,
  benchmark results at `benchmarks/slice-8-results-2026-04-22.csv`.
- **Test status**: 1744 PASS / 0 FAIL / 1 SKIP (pre-existing S4
  dispatch skip from Task 1's helper) / 1 WARN (pre-existing scran SVD
  notice, unchanged since Slice 0).
- **Check status**: `devtools::check(error_on = "note")` exits 1 due
  to 2 structural notes (`benchmarks/` top-level dir added this slice;
  6.1 MB installed package size). Underlying `R CMD check` is clean of
  errors and warnings.
- **Public surface**: 110 exports, unchanged from Slice 7 (Slice 8
  added kernel internals via cpp11, no new exports).

## Overall progress map

After Slice 8, the design-spec core surface is ~95% delivered. What
remains is tail: perf parity, Julia interop details, niche backends.

## Slice 9 scope — user directive

User directive at Slice 8 exit (2026-04-22): **"fix all discrepancies"
and "make sure we are as fast as the julia code"**. Two workstreams,
both first-tier:

### Workstream A — Fix discrepancies

1. **R vs Julia grouped-matrix operator semantic inversion** (material).
   Slice 8 discovered that R's dispatch treats:
   - G2 = `GroupRowsBy + ReduceToColumn` (producing `ngroups × ncol`)
   - G3 = `GroupColumnsBy + ReduceToRow` (producing `nrow × ngroups`)

   Julia DAF treats the operators opposite:
   - G2 = `GroupRowsBy + ReduceToRow`
   - G3 = `GroupColumnsBy + ReduceToColumn`

   R's convention was inherited from pre-Slice-8 code. Aligning with
   Julia requires:
   - Swapping `is_g2`/`is_g3` dispatch conditions in
     `R/query_eval.R` `.apply_reduction_grouped_matrix`.
   - Updating G4a/G4b inner-op derivation accordingly.
   - Rewiring all Task-10 grouped-matrix tests in
     `tests/testthat/test-query-grouped-slice8.R` (currently 84
     assertions).
   - Re-checking any pre-Slice-8 grouped tests in
     `tests/testthat/test-query-eval-groupby.R` for semantic drift.
   - Running the extended Julia-queries fixture (item 2 below) to
     confirm byte parity lands after the swap.

2. **Extend Julia-queries fixture for Slice 8 surface.** After the
   semantic swap, extend `tests/testthat/fixtures/julia-queries/fixture.json`
   via `dev/scripts/regen-julia-queries-fixture.jl` with records for:
   - Grouped reductions (G1, G2, G3 — each builtin op + parametric).
   - Mode-on-character (once G1's `>>` operator-syntax mismatch is
     resolved — may require extending R's parser to accept the Julia
     grouped-vector syntax, OR accepting a translation layer).
   - Convert-sparse-to-int/logical (blocked until R accepts Julia's
     `Float32`/`Int32`/`Bool` type vocabulary, OR a translation layer
     is added).

3. **Grouped-matrix G1 syntax gap.** R's parser currently rejects
   Julia's `>>` operator. Options: extend parser to accept `>>` as an
   alias for G1 reduction, OR keep R's `>-` for G1 and document
   divergence. Decide at Slice 9 kickoff.

4. **Convert type-name vocabulary mismatch** (pre-existing from
   Slice 7). Julia uses `Float32`/`Int32`/`Bool`; R uses
   `double`/`integer`/`logical`. Options: extend R's accepted type
   names to include Julia's, add a bidirectional alias table, OR
   accept divergence. Decide at Slice 9 kickoff.

5. **Axis-rename view re-apply** (minor, from Slice 8 Task 13).
   `complete_daf` correctly re-applies identity views, but the
   renamed-axis case is not tested because `viewer()` doesn't support
   the `"= renamed_cell"` query form. Investigate whether rename is
   supported via a different arg shape; add a renamed-axis round-trip
   test or document the limitation.

### Workstream B — Performance parity with DAF.jl

This is a new first-tier goal. Approach: **measure, then target**.

1. **Build a bake-off harness** that runs the same query on both
   `dafr` (native R) and DAF.jl (Julia) against the same `FilesDaf`
   data (the existing `example_cells_daf()` dataset is a good start
   since it's already round-trip-verified). Measure wall time for
   each via `bench::mark` (R side) and `BenchmarkTools.jl` (Julia
   side). Emit a comparison CSV listing `query | dafr_time |
   julia_time | ratio (julia/dafr)`.

   Live in `benchmarks/` (package repo) + `dev/benchmarks/` for the
   CSV output. The harness should be idempotent and reproducible
   across machines.

2. **Query set for the bake-off**: start with every query in the
   existing julia-queries fixture (~28 records after Slice 7), plus:
   - Heavy reductions on a 10k × 10k sparse `dgCMatrix` (post-
     `copy_all` from `example_cells_daf`).
   - Grouped reductions on the same (after Workstream A lands).
   - FilesDaf open + read a full matrix (I/O + mmap path).
   - Chain reader with 2–3 layers.
   - A `complete_chain` + `complete_daf` round-trip.

3. **Identify gaps.** For every query where `ratio = julia_time /
   dafr_time < 0.7` (Julia is >1.4× faster), open a sub-task to
   investigate. Likely categories:
   - **Dispatch overhead**: R's S7 multi-dispatch is slower than
     Julia's multiple dispatch. For fine-grained queries this adds
     up. Mitigation: cache dispatch lookups, inline hot paths.
   - **Parser overhead**: R's recursive-descent query parser may be
     slower than Julia's. Profile, find bottlenecks.
   - **Copy overhead**: `as.matrix()`, `as.integer()`, R's
     copy-on-modify. Identify unnecessary copies.
   - **I/O path**: mmap + ALTREP vs Julia's `mmap`-backed arrays.
     Compare FilesDaf read times directly.

4. **Address each gap**. Each becomes its own sub-slice (Slice 9a,
   9b, …) or bundled fixes. Be honest about which paths are
   realistically matchable:
   - **Matchable**: kernel-bound ops (already competitive per
     Slice 8), BLAS-backed dense ops, mmap I/O.
   - **Likely matchable with effort**: CSC iteration, FilesDaf
     read dispatch.
   - **Hard to match**: query parse + dispatch for light queries
     (Julia has JIT + multi-dispatch; R has AST walk + S7). Document
     irreducibility, aim for "within 2×" on short queries.

5. **Fix G3 kernel memory at scale** (from Slice 8 deferred). The
   thread-bucket layout is O(nthreads × nrow × ngroups). At 128
   threads × 10k × 100 this is 6.7 GB and makes the "fast" path
   *slower* than baseline. Options:
   - Row-partitioned fallback: at large nrow, partition rows across
     threads instead of duplicating the full bucket.
   - Adaptive thread cap: if `nthreads × nrow × ngroups × sizeof(Acc)
     > memory_budget`, reduce `nthreads` for this call.
   - Sequential fallback: at extreme sizes, fall back to
     single-threaded.

   This is load-bearing for making `dafr` usable on the lab's
   128-core machines at metacell scale.

### Suggested slice structure

Option I: **one large Slice 9** covering both workstreams (similar
to Slice 8's shape — 15–20 tasks, 6–8 days).

Option II: **split into Slice 9a (discrepancies) + Slice 9b (perf
parity)** — clearer exit gates, separate merge points. 9a closes the
Julia-parity correctness issues first so the bake-off in 9b runs on
semantically-aligned output.

Recommend Option II. 9a establishes the correctness baseline; 9b
then profiles and optimizes with confidence that we're comparing
equivalent operations.

## Decision points for Slice 9 kickoff

At kickoff, the user decides:

1. **Semantic swap for grouped-matrix operators?** (Yes recommended —
   aligns with Julia, prerequisite for byte-parity fixture records.)
2. **G1 `>>` operator support?** (Extend R parser, add translation,
   or accept divergence with `>-`.)
3. **Convert type vocabulary?** (Julia aliases, bidirectional, or
   accept divergence.)
4. **Slice structure:** one slice (I) or two (II)?
5. **Perf parity realistic target:** blanket (every query ≤ 1.0×
   Julia), hot-path (kernels + I/O only), or tiered (specific gates
   per op category)?
6. **G3 kernel fix:** row-partition, adaptive thread cap, or
   sequential fallback — pick one based on profile data.

## Still deferred after Slice 8

Carried forward, not targeted by Slice 9 directives:

- `bestify` heuristic for `copy_vector` / `copy_matrix`.
- `reconstruct_axis` with a pre-existing target axis.
- H5df / AnnData / Zarr backends.
- Long-vector (>2³¹) ALTREP scenarios.
- UInt32 > 2³¹ read arm (Slice-2 inherited).
- Multi-writer filesystem locking on FilesDaf.
- `computation()` dual-/triple-contract forms.
- `@examples` for the ~25 skipped exports.

## Known mines for Slice 9

From Slice 8 exit:

- `.apply_reduction_grouped_matrix` dispatch conditions at
  `R/query_eval.R` are inverted vs Julia. Any fix here cascades
  through G4 decomposition and test expectations.
- `kernel_grouped_reduce_csc_cpp` axis=3 thread-bucket memory is
  O(nthreads × nrow × ngroups). FIXME comment present at the
  allocation site.
- `kernel_grouped_quantile_csc_cpp` and `kernel_grouped_mode_csc_cpp`
  axis=3 paths share the same memory concern.
- `Acc::push` in `src/kernel_grouped_acc.h` unconditionally computes
  `log(v + eps)` even for non-GeoMean ops (minor perf waste; ~5M
  wasted log calls on a 10k × 10k × 5% nnz input).
- `derive_op` silently returns 0 for unknown op strings (typo-safety
  concern).
- `.op_convert` sparse→character still densifies (no sparse character
  class in R; accepted).

## Repo conventions (reinforced across Slices 0–8)

- **4-space R indent** (post-Slice-3 styler pass). No tabs.
- **S7 multi-dispatch always uses `list(ClassA, ...)` signatures.**
- **`#' @include` directives are load-bearing** for S7 method registration.
- **`format_get_*` returns plain arrays without dimnames**; `get_*` adds names.
- **`sort(..., method = "radix")`** for all listing returns.
- **`.assert_name(x, "x")`** / **`.assert_flag(x, "x")`** at public-API boundary.
- **`.DAFR_UNDEF` sentinel** + **`.is_undef`** for optional-default args.
- **`sQuote()`** around names in error messages.
- **`.dafr_builtin` attribute** on default-op functions.
- **`%||%`** lives in `R/utils.R:1`; do not redefine.
- **No emojis.** Never `--no-verify` / `--amend` / force-push. Always NEW commits.
- **Native C++ headers use `.h`, not `.hpp`.**
- **cpp11 (NOT Rcpp)** for all C++ bindings.
- **Julia fixture scripts use an inline minimal JSON emitter** (no Manifest.toml).
- **Kernel naming**: `kernel_<op>_<layout>.cpp` with `[[cpp11::register]]`
  entry points suffixed `_cpp`. OpenMP via `openmp_shim.h` helpers
  (`DAFR_PARALLEL_FOR`, `dafr_omp_get_*`), never raw pragmas.
- **`dafr.kernel_threshold` option** (default 1024L) registered in
  `R/options.R` via `.dafr_default_options`; access via
  `.dafr_kernel_threshold()` which wraps `dafr_opt()`.
- **Formula authority**: `R/operations.R` `.op_*` functions are
  authoritative for every op's formula and edge-case behaviour.
  Kernels MUST match. If a plan diverges, match the R impl, not the plan.

## Auto-memory carry-over

Durable feedback (see `~/.claude/projects/-.../memory/MEMORY.md`):

- **Slice-4 P3 TDD divergence** — bundled-commit with all-tests-green acceptable.
- **Native port motivation + perf parity goal** — was "escape version pain, not perf";
  user added "match DAF.jl speed" as a first-tier goal on 2026-04-22.
- **Model selection — use Opus freely** — Opus for design-heavy implementation,
  final whole-branch reviews, or speed-sensitive work.
- **L2 upstream PR declined** — do NOT re-raise at slice exits.

## Julia DAF state at Slice 8 exit

- `~/src/DataAxesFormats.jl` at
  `49fbba140437387a378217c2fa658d4231d0c8c1` (unchanged since Slice 3 —
  six slices of stability now).
- `~/src/TanayLabUtilities.jl` at `48a4a57` (unchanged).
- Both registered as Julia `dev` packages in conda env `dafr-mcview`
  (Julia 1.12.5 in the conda env).
- Five fixture sets in the package repo, unchanged by Slice 8:
  - `tests/testthat/fixtures/julia-queries/` (Slice 3 + Slice 7 extensions)
  - `tests/testthat/fixtures/julia-chains/` (Slice 4)
  - `tests/testthat/fixtures/julia-adapter/` (Slice 5)
  - `tests/testthat/fixtures/julia-copies/` (Slice 6)

## Ready-to-paste prompt for Slice 9

> Start implementing Slice 9 of the native-R `dafr` package.
>
> - Package repo: `~/src/dafr-native/` on branch `main`, tag `slice-8`
>   marks the Slice 8 merge commit (`f7978cc`).
> - Dev repo: `~/src/dafr-native/dev/` — separate nested git repo.
> - Kickoff breadcrumb: `~/src/dafr-native/dev/notes/slice-9-kickoff.md`
>   (this document).
> - Slice 8 exit note: `~/src/dafr-native/dev/notes/slice-8-exit.md`.
>
> **Scope directive (user, 2026-04-22):** "fix all discrepancies" and
> "make sure we are as fast as the julia code". Two workstreams —
> correctness (A) and perf parity (B) — detailed in the kickoff's
> "Slice 9 scope" section. Recommend split into 9a (discrepancies)
> and 9b (perf parity). Decide at kickoff.
>
> Use `superpowers:brainstorming` to lock scope + decision points
> (#1–#6 in the kickoff), then `superpowers:writing-plans` to draft
> the slice plan, then `superpowers:subagent-driven-development` to
> execute.
>
> **Model selection:** Opus for design-heavy implementation dispatches
> (semantic-swap rewrite, bake-off harness design, G3 kernel redesign)
> and the final whole-branch review. Sonnet for mechanical per-op
> kernel tweaks and test rewrites.
>
> **Mines to brief the Slice 9 agent:** see the "Known mines for
> Slice 9" section of this kickoff. Short list:
> - Grouped-matrix op semantic inversion at
>   `R/query_eval.R` `.apply_reduction_grouped_matrix` is_g2/is_g3.
> - G3 kernel thread-bucket memory at high thread counts — fix before
>   shipping perf claims.
> - Formula authority rule: `R/operations.R` `.op_*` is the source of
>   truth; the Slice 8 plan had VarN and Mode tiebreak bugs that the
>   implementer caught by cross-checking.
> - Slice-8 test `test-query-grouped-slice8.R` (84 assertions) will
>   need inversion when the semantic swap lands. Budget time for this.
>
> **Julia DAF state at handoff:** `~/src/DataAxesFormats.jl` at
> `49fbba140437387a378217c2fa658d4231d0c8c1` (unchanged since Slice 3
> — six slices). `~/src/TanayLabUtilities.jl` at `48a4a57`. Before
> regenerating any fixture, check whether DAF.jl has moved.
