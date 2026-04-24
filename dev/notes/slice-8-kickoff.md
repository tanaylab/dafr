# Slice 8 — Kickoff breadcrumb

**Date:** 2026-04-21.
**Predecessor:** Slice 7 (tag `slice-7` on `main` — to be applied to the
merge commit by the user), exit gate at `dev/notes/slice-7-exit.md`.

## What changed between Slice 6 exit and now

Slice 7 (branch `slice-7-ops-expansion`) added 12 new default query ops — 4
eltwise (`Clamp`, `Convert`, `Fraction`, `Significant`) and 8 reductions
(`Var`, `Std`, `VarN`, `StdN`, `Median`, `Quantile`, `GeoMean`, `Mode`) — all
R-only, registered at package load via `.register_default_ops()`. The
Julia-queries fixture was extended by 11 byte-parity records (28 total); end-
to-end query tests were added for every new op. No C++ was needed; all ops
are pure R on top of the existing Slice-3/4 dispatch infrastructure.

## Current state (as of this writing)

- **Package repo**: `/home/aviezerl/src/dafr-native/`, branch
  `slice-7-ops-expansion` at `083559d`, clean. Awaiting user merge to `main`
  and tag `slice-7`. Will be in sync with `origin/main` after merge + push.
- **Dev repo** (nested, gitignored by package repo):
  `/home/aviezerl/src/dafr-native/dev/`, branch `main`. Exit + kickoff
  notes committed here.
- **Test status**: 1448 PASS / 0 FAIL / 0 SKIP / 1 WARN (pre-existing
  scran/irlba SVD tolerance notice in `test-altrep-downstream.R`, unchanged
  since Slice 0).
- **Check status**: `devtools::check(error_on = "note")` with
  `_R_CHECK_SYSTEM_CLOCK_=0` — 0 ERROR / 0 WARNING / 0 NOTE.
- **Public surface**: 110 exports (98 after Slice 6 + 12 new in Slice 7:
  `Clamp`, `Convert`, `Fraction`, `Significant`, `Var`, `Std`, `VarN`,
  `StdN`, `Median`, `Quantile`, `GeoMean`, `Mode`).

## Overall progress map

Design spec at `dev/specs/2026-04-19-native-r-dafr-design.md` §15
originally listed 8 slices. Delivered surface after Slice 7:

| Original slice goal | Status |
|---|---|
| Slice 0: scaffold, ALTREP POC, FilesDaf spec draft | **DONE** (tag `slice-0`) |
| Slice 1: MemoryDaf + scalar/vector/matrix + axes + cache | **DONE** (tag `slice-1`) |
| Slice 2: FilesDaf + mmap + Julia compat | **DONE** (tag `slice-2`) |
| Slice 3: query tokenizer/parser/AST/evaluator + eltwise/reductions | **DONE** (tag `slice-3`) |
| Slice 4: full query DSL + QueryData cache | **DONE** (merged into `slice-3`) |
| Slice 5: Views + Chains + ReadOnly + axis aliasing | **DONE** (Views in `slice-3`, Chains in `slice-4`) |
| Slice 6: Contracts + Computations + Adapters | **DONE** (Contracts in `slice-4`; Computations + Adapters in `slice-5`) |
| Slice 7: Concat + Reconstruction + Copies + Complete + Groups + ExampleData | **DONE** (Groups in `slice-3`; ExampleData in `slice-5`; Copies + Concat + Reconstruction + Complete in `slice-6`) |
| Slice 7 ops expansion (Option C): 12 new default ops | **DONE** (tag `slice-7`) |
| Slice 8 (deferred): AnnData interop + Zarr backend + tail work | Not started |

Roughly **95% of the design spec's core surface is delivered.** Remaining
work is tail: niche integrations (AnnData/Zarr), `bestify` perf polish,
fast paths for new ops, and a handful of deferred corner cases.

## Slice 8 scope — NOT locked

The user will decide at Slice 8 kickoff. Candidates from the deferred list,
with brief size estimates:

- **AnnData interop** — large (1-2 weeks). Requires a `{SingleCellExperiment}`
  or similar Bioconductor bridge; bidirectional h5ad reader/writer. Scope
  creep risk; block on whether the user actually needs this.
- **Zarr backend for `open_daf`** — multi-day. `{Rarr}` exists but Julia uses
  DAF's own Zarr writer (different layout). Needs layout spec reverse-
  engineering before implementation.
- **`bestify` heuristic** — small (1-2 days). Sparse-vs-dense nnz-based
  promotion in `copy_vector` / `copy_matrix`. Revisit only after profiling.
- **Matrix-kernel fast paths for Slice-7 ops** — small (1-2 days).
  `matrixStats::rowVars` / `rowSds` / `rowMedians` / `rowQuantiles` +
  sparse-specialised dispatch for reduction ops. Profile first.
- **Long-vector (>2³¹) ALTREP** — unknown scale. Blocks metacell-scale data
  at extreme sizes; only relevant if users hit 2B+ element vectors.
- **`complete_daf` + `base_daf_view` JSON re-apply** — small (1-2 days).
  JSON is stored/parsed but the view is not re-applied on reopen. Finishes
  the Slice-6 `complete_daf` story.

## Still deferred after Slice 7

Carried forward from Slice 6 + Slice 7 additions:

- `bestify` heuristic for `copy_vector` / `copy_matrix`.
- `reconstruct_axis` with a pre-existing target axis (`properties_defaults`
  path).
- `complete_daf` + `base_daf_view`: JSON stored/parsed but view not re-applied
  on reopen.
- H5df backend for `open_daf`.
- AnnData interop / Zarr backend.
- `computation()` dual-/triple-contract forms (Julia-UNTESTED upstream).
- Long-vector (>2³¹) ALTREP scenarios.
- UInt32 > 2³¹ read arm (Slice-2 inherited).
- Multi-writer filesystem locking on FilesDaf (Slice-2 inherited).
- `@examples` for the 25 skipped exports (string constants + S7 class names).
- **Matrix-kernel fast paths for Slice-7 ops** (matrixStats row*): profile
  first; add only if matrix-heavy queries are slow in practice.
- **Mode on character input**: requires refactoring `.apply_reduction_grouped_*`
  off the `vapply(..., numeric(1))` contract.
- **`type` parameter on ops**: Julia's per-op `type` is not ported.
- **Convert in the Julia fixture**: type-name vocabulary mismatch (R:
  `double`/`integer`/`logical` vs Julia: `Float64`/`Int32`/`UInt32`/`Bool`).

## Known mines for Slice 8

From Slice 7 exit (full detail in `dev/notes/slice-7-exit.md`):

- `.apply_reduction_grouped_*` uses `vapply(..., numeric(1))`. Any future
  char-valued op must refactor this.
- `.cast_matrix_type("integer", dgCMatrix)` dense-coerces. Now reachable
  from `Convert` for the first time; still unexercised by tests.
- **`.matrix_type_ok` missing `character` case** — pre-existing Slice-4 mine.
  Still open.
- Julia-side `significant!` underflows on UInt32 inputs (see exit note).
  The fixture routes around this; if DAF.jl moves past `49fbba1` and fixes it,
  the fixture workaround can be simplified.
- From Slice 6: `.cast_matrix_type("integer", dgCMatrix)` dense-coerces;
  `concatenate` string-only prefix narrowing; `copy_all` axis-collision is
  lazy; `reconstruct_axis` `vapply` risk on empty-implicit first entry.

## Repo conventions (reinforced across Slices 0–7)

- **4-space R indent** (post-Slice-3 styler pass). No tabs.
- **S7 multi-dispatch always uses `list(ClassA, ...)` signatures.**
- **`#' @include` directives are load-bearing** for S7 method registration.
- **`format_get_*` returns plain arrays without dimnames**; `get_*` adds names.
- **`sort(..., method = "radix")`** for all listing returns.
- **`.assert_name(x, "x")`** / **`.assert_flag(x, "x")`** at public-API boundary.
- **`.DAFR_UNDEF` sentinel** + **`.is_undef`** for optional-default args.
- **`sQuote()`** around names in error messages.
- **`.dafr_builtin` attribute** on default-op functions (identity hook for
  P2/P3/P4). New default ops MUST carry this via `.register_default_ops()`.
- **`%||%`** lives in `R/utils.R:1`; do not redefine.
- **No emojis.** Never `--no-verify` / `--amend` / force-push. Always NEW commits.
- **Native C++ headers use `.h`, not `.hpp`.**
- **Julia fixture scripts use an inline minimal JSON emitter** (Slices 3–7 precedent).

## Auto-memory carry-over

Durable feedback (see `~/.claude/projects/-.../memory/MEMORY.md`):

- **Slice-4 P3 TDD divergence** — bundled-commit with all-tests-green acceptable.
- **Native port motivation** — escape Julia+R version-management pain, not perf.
- **Model selection — use Opus freely** — Opus for design-heavy implementation,
  final whole-branch reviews, or speed-sensitive work.
- **L2 upstream PR declined** — do NOT re-raise at slice exits.

## Julia DAF state at Slice 7 exit

- `~/src/DataAxesFormats.jl` at
  `49fbba140437387a378217c2fa658d4231d0c8c1` (unchanged since Slice 3 —
  five slices of stability now).
- `~/src/TanayLabUtilities.jl` at `48a4a57` (unchanged).
- Both registered as Julia `dev` packages in conda env `dafr-mcview`
  (Julia 1.12.5 in the conda env).
- Five fixture sets in the package repo:
  - `tests/testthat/fixtures/julia-queries/` (Slice 3, extended Slice 7)
  - `tests/testthat/fixtures/julia-chains/` (Slice 4)
  - `tests/testthat/fixtures/julia-adapter/` (Slice 5)
  - `tests/testthat/fixtures/julia-copies/` (Slice 6)

## Ready-to-paste prompt for Slice 8

> Start implementing Slice 8 of the native-R `dafr` package.
>
> - Package repo: `~/src/dafr-native/` on branch `main`, tag `slice-7`
>   marks the Slice 7 exit (merge commit, to be applied by the user). In
>   sync with origin after the merge + push.
> - Dev repo: `~/src/dafr-native/dev/` — separate nested git repo.
> - Kickoff breadcrumb: `~/src/dafr-native/dev/notes/slice-8-kickoff.md`
>   (this document).
> - Slice 7 exit note: `~/src/dafr-native/dev/notes/slice-7-exit.md`.
>
> **Scope:** NOT locked — decide at kickoff. Primary candidates: `complete_daf`
> JSON re-apply (small), matrix fast paths for Slice-7 ops (small), AnnData
> interop (large). See "Slice 8 scope" section of this kickoff.
>
> Use `superpowers:writing-plans` to draft the Slice 8 plan once scope is
> decided, then `superpowers:subagent-driven-development` to execute it.
>
> **Model selection:** Opus for design-heavy implementation dispatches and
> the final whole-branch review. Sonnet for mechanical per-op work.
>
> **Mines to brief the Slice 8 agent:** `.apply_reduction_grouped_*` uses
> `vapply(..., numeric(1))` — any char-valued op must refactor this.
> `.cast_matrix_type("integer", dgCMatrix)` dense-coerces (now reachable
> from `Convert`). `.matrix_type_ok` missing `character` case (pre-existing
> Slice-4 mine). Julia-side `significant!` underflows on UInt32 inputs;
> fixture routes around it.
>
> **Julia DAF state at handoff:** `~/src/DataAxesFormats.jl` at
> `49fbba140437387a378217c2fa658d4231d0c8c1` (unchanged since Slice 3 —
> five slices). `~/src/TanayLabUtilities.jl` at `48a4a57`. Both registered
> as Julia `dev` packages in `dafr-mcview`. Before regenerating any fixture,
> check whether DAF.jl has moved.
