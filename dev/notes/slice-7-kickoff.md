# Slice 7 — Kickoff breadcrumb

**Date:** 2026-04-21.
**Predecessor:** Slice 6 (tag `slice-6` on `main` at commit `e38c53a`),
exit gate at `dev/notes/slice-6-exit.md`.

## What changed between Slice 5 exit and now

1. **Slice 6 landed.** 29 commits on `slice-6-copies-concat-complete`,
   merged fast-forward into `main`, tag `slice-6` applied. +3077 /
   −138 across 36 files. Net by phase:
   - Phases A–E (copies): `copy_scalar`, `copy_axis`, `copy_vector`,
     `copy_matrix`, `copy_tensor`, `copy_all`, `empty_data()` +
     `MERGE_*` constants. R port of Julia `Copies.jl`. Sparse-preserving
     pad-mode via `Matrix::sparseMatrix` embedding.
   - Phase F (adapter refactor): `adapter()` now calls `copy_all` with
     `insist = FALSE`; `.copy_view_to_daf` removed. Sparse pad-mode
     regression test at adapter level.
   - Phase G (concatenate): `concatenate()` with 1+ axes, `dataset_axis`,
     prefix heuristic (widened from plan to match Julia parity), merge
     actions (`SkipProperty` / `LastValue` / `CollectAxis`).
   - Phase H (complete): `complete_chain`, `complete_daf`, `open_daf`
     (FilesDaf only; H5df deferred).
   - Phase I (reconstruction): `reconstruct_axis` (core behaviors).
   - Phase J (fixture): `tests/testthat/fixtures/julia-copies/` —
     byte-parity roundtrip for `copy_all` and `concatenate` against
     DAF.jl `49fbba1…` (still pinned since Slice 3).
   - Phase Z (polish): NEWS, NAMESPACE, Collate, exit note. Two
     check-regressions fixed in-phase (em-dash encoding; bare `as()`).

2. **Slice-5 dense-coercion mine CLOSED.** The kickoff-flagged mine
   (`.copy_view_to_daf`'s `as.matrix(val)` at ~12GB scale) is gone;
   the new `.embed_matrix_in_pad` preserves sparsity end-to-end.
   Regression test: `expect_s4_class(result, "dgCMatrix")` at both
   `copy_matrix` and `adapter` levels.

3. **Pushed.** Slices 5 and 6 (and tags `slice-5` / `slice-6`) are now
   on `origin/main`. Local is in sync.

## Current state (as of this writing)

- **Package repo**: `/home/aviezerl/src/dafr-native/`, branch `main` at
  `e38c53a`, clean, in sync with `origin/main`. Tags locally and on
  origin: `slice-0` … `slice-6`.
- **Dev repo** (nested, gitignored by package repo):
  `/home/aviezerl/src/dafr-native/dev/`, branch `main` at `a04b1d1`
  (Slice 6 exit commit). Pre-existing untracked files from prior slices
  untouched.
- **Test status**: `testthat::test_dir("tests/testthat")` —
  **1315 PASS / 0 FAIL / 0 SKIP / 1 WARN** at `slice-6`. The WARN is
  still the pre-existing `scran::quickCluster` / `irlba::irlba` SVD
  tolerance notice in `test-altrep-downstream.R`, unchanged since
  Slice 0.
- **Check status**: `devtools::check(error_on = "note")` with
  `_R_CHECK_SYSTEM_CLOCK_=0` — **0 ERROR / 0 WARNING / 0 NOTE**.
- **Build status**: `pkgbuild::compile_dll(debug = FALSE)` clean. No
  new C++ files in Slice 6.
- **Public surface**: 98 exports (83 after Slice 5 + 15 new in Slice 6:
  `copy_scalar`, `copy_axis`, `copy_vector`, `copy_matrix`,
  `copy_tensor`, `copy_all`, `empty_data`, `concatenate`,
  `complete_chain`, `complete_daf`, `open_daf`, `reconstruct_axis`,
  `MERGE_SKIP`, `MERGE_LAST_VALUE`, `MERGE_COLLECT_AXIS`).

## Overall progress map

Design spec at `dev/specs/2026-04-19-native-r-dafr-design.md` §15
originally listed 8 slices. Delivered surface after Slice 6:

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
| Slice 8 (deferred): AnnData interop + Zarr backend | Deferred |

Roughly **90% of the design spec's core surface is delivered**. What
remains is tail functionality: op coverage beyond the current 10
defaults; niche integrations (AnnData/Zarr); `bestify` perf polish.

## Slice 7 scope (locked at Slice 6 kickoff)

User decision at Slice 6 kickoff: Slice 7 = **Option C — Ops
expansion**. Remaining ~20 Julia ops to add under the existing
`register_eltwise` / `register_reduction` surface:

- **Eltwise**: `Clamp`, `Convert`, `Fraction`, `Significant`, `Type`,
  plus any stragglers surfaced during implementation.
- **Reductions**: `GeoMean`, `Median`, `Quantile`, `Std`, `StdN`,
  `Var`, `VarN`, `All`, `Any`, plus any stragglers.
- Default-op registrations via `.register_default_ops()` at package
  load.
- Test matrix: one test per op × input type (dense / sparse / logical
  where applicable). Matches the existing Slice-4 P2/P3/P4 test
  pattern.
- C++ kernels: **start R-only.** Add C++ (via the Slice-4 P4
  fused-kernel pattern) only for ops where profiling warrants it. The
  kickoff estimate of 2400 C++ LoC is an upper bound, likely much
  less delivered.

**Estimated scope:** ~1500 R + optional C++ LoC. Mostly mechanical
one-method-per-op work. Perf-flavored slice.

## Still deferred after Slice 7

From Slice 6 exit's "Still deferred" section, carried forward:

- `bestify` heuristic for `copy_vector` / `copy_matrix` (sparse-vs-dense
  promote/demote by nnz). Revisit if profiling warrants.
- `reconstruct_axis` with a pre-existing target axis
  (`properties_defaults` path).
- `complete_daf` + `base_daf_view`: JSON is stored/parsed but the view
  is not re-applied on reopen.
- H5df backend for `open_daf`. Originally Slice 8.
- AnnData interop / Zarr backend. Originally Slice 8.
- `computation()` dual-/triple-contract forms (Julia-UNTESTED upstream).
- Long-vector (>2³¹) ALTREP scenarios.
- UInt32 > 2³¹ read arm (Slice-2 inherited).
- Multi-writer filesystem locking on FilesDaf (Slice-2 inherited).
- `@examples` for the 25 skipped exports (string constants + S7 class
  names). Low-value.

## Known mines laid in Slice 6 for Slice 7

(Full list in `dev/notes/slice-6-exit.md`. Key ones to brief Slice 7):

- **`copy_all` does not infer tensor keys from matrix names.** If Slice
  7 adds a `Type`-converting op that operates on tensor-style
  (`<entry>_<base>`) matrices, users will need to call `copy_tensor`
  explicitly.
- **`copy_all` axis-collision is LAZY** (raises "disjoint entries" at
  vector/matrix copy time, not "already exists in destination" eagerly
  at axis time like Slice-5's `.copy_view_to_daf` did). New semantics;
  cleaner than Slice 5 but different.
- **`.cast_matrix_type("integer", dgCMatrix)`** dense-coerces. Only
  triggered if a user requests integer type on a sparse source matrix.
  Not exercised by any current test. If Slice 7's `Type` op targets
  sparse matrices, plan to avoid this path or add coverage.
- **`concatenate` string-only prefix narrowing**: integer-keyed
  cross-axis properties would silently not-prefix. Documented.
- **`reconstruct_axis` `vapply(..., FUN.VALUE = values[[1L]])`** risk
  if the first entry is empty-implicit. Accepted limitation.
- **`.concat_axis_matrix` transposes via `Matrix::t()` which allocates.**
  Fine at fixture scale; watch at metacell scale.
- **Phase B and Phase H commits have understated titles** — git log
  alone does not tell the full story; `dev/notes/slice-6-exit.md`
  carries the audit trail.
- **`.matrix_type_ok` missing `character` case** (pre-existing
  Slice-4 mine; unchanged since Slice 4). If Slice 7 adds any op that
  returns a character matrix, this mine may fire.

## Repo conventions (reinforced across Slices 0–6)

- **4-space R indent** (post-Slice-3 styler pass).
- **S7 multi-dispatch always uses `list(ClassA, ...)` signatures.**
- **`#' @include` directives are load-bearing** for S7 method
  registration. Empirical test (`devtools::document()` leaves Collate
  byte-identical) is the ground-truth convention — trim leaf files
  from includer lists only when the Collate stays stable.
- **`format_get_*` returns plain arrays without dimnames**; `get_*`
  user-facing wrappers add names.
- **`sort(..., method = "radix")`** for all listing returns.
- **`.assert_name(x, "x")`** / **`.assert_flag(x, "x")`** validate all
  name- and flag-like args at the public-API boundary. Every new
  export should follow this pattern.
- **`.DAFR_UNDEF` sentinel** + **`.is_undef`** distinguish "no default
  given" (raise) from "default = NULL" (silent skip) across the
  `copy_*` family. Slice 7 ops don't need this, but if any new HOF
  acquires a `default` arg, reuse the pattern.
- **`sQuote()`** around names in error messages — established
  throughout the codebase.
- **`.dafr_builtin` attribute** on default-op functions is the identity
  hook used by P2/P3/P4. **Slice 7's new default ops MUST carry this
  attribute** via `.register_default_ops()`.
- **`%||%`** (null-coalesce) lives in `R/utils.R:1`. Available
  package-wide; do not redefine.
- **Shell aliases `rm`/`cp` are interactive** — use `/bin/rm` /
  `/bin/cp` if scripting destructive operations.
- **No emojis.**
- **Never `--no-verify` / `--amend` / force-push.** Always NEW commits.
- **Never add "Generated with Claude Code" footer or co-author line**
  to commits or PR bodies.
- **Native C++ headers use `.h`, not `.hpp`.**
- **Julia fixture scripts use an inline minimal JSON emitter** (Slice-3
  + Slice-4 + Slice-5 + Slice-6 precedent).
- **User global CLAUDE.md** says no sycophancy, contradict when needed.

## Auto-memory carry-over (applies from the start of Slice 7)

Durable feedback stored across conversations (see
`~/.claude/projects/-.../memory/MEMORY.md`):

- **Slice-4 P3 TDD divergence** — bundled-commit implementation with
  all-tests-green and preserved audit trail is acceptable.
- **Native port motivation** — dafr-native exists to escape Julia+R
  version-management pain, not for perf/architecture. Scope with that
  lens.
- **Model selection — use Opus freely** — don't default to Sonnet on
  cost grounds. Pick Opus for design-heavy implementer dispatches and
  final whole-branch reviews. Slice 6 used Opus for Phase C
  (copy_matrix / sparse-embed) and Phase G (concatenate) and the final
  review; Sonnet handled the mechanical phases cleanly.
- **L2 upstream PR declined** — do NOT re-raise the
  `filesdaf-spec-draft.md` → `tanaylab/DataAxesFormats.jl` docs PR at
  Slice 7 exit. User has declined 5× already.

## Julia DAF state at Slice 6 exit

- `~/src/DataAxesFormats.jl` at
  `49fbba140437387a378217c2fa658d4231d0c8c1` (unchanged since Slice 3
  — four slices of stability now).
- `~/src/TanayLabUtilities.jl` at `48a4a57`.
- Both registered as Julia `dev` packages in conda env `dafr-mcview`
  (Julia 1.12.5 in the conda env; system Julia is 1.11.8).
- Four fixture sets in the package repo:
  - `tests/testthat/fixtures/julia-queries/` (Slice 3)
  - `tests/testthat/fixtures/julia-chains/` (Slice 4)
  - `tests/testthat/fixtures/julia-adapter/` (Slice 5)
  - `tests/testthat/fixtures/julia-copies/` (Slice 6)
- Slice 7 (ops) **may not need a Julia fixture**: the existing
  `julia-queries` fixtures already exercise query-eval end-to-end.
  If any Slice-7 op produces values that differ from what the existing
  ops produce in Julia, a small fixture extension is warranted. Before
  regenerating, check if DAF.jl has moved:
  `git -C ~/src/DataAxesFormats.jl pull --ff-only`.

## Ready-to-paste prompt for Slice 7

> Start implementing Slice 7 of the native-R `dafr` package.
>
> - Package repo: `~/src/dafr-native/` on branch `main`, tag `slice-6`
>   marks the Slice 6 exit at commit `e38c53a`. In sync with origin.
> - Dev repo: `~/src/dafr-native/dev/` — separate nested git repo.
> - Kickoff breadcrumb: `~/src/dafr-native/dev/notes/slice-7-kickoff.md`
>   (this document).
> - Slice 6 plan (fully executed):
>   `~/src/dafr-native/dev/plans/2026-04-21-slice-6-copies-concat-complete-reconstruction.md`.
> - Slice 6 exit note: `~/src/dafr-native/dev/notes/slice-6-exit.md`.
>
> **Scope:** Option C — Ops expansion. Remaining ~20 Julia ops (see
> "Slice 7 scope" section of this kickoff). Start R-only; add C++
> kernels only where profiling warrants.
>
> Use `superpowers:writing-plans` to draft the Slice 7 plan, then
> `superpowers:subagent-driven-development` to execute it.
>
> **Model selection:** Opus for implementer dispatches involving real
> design judgment (choosing fast paths, deciding sparse-vs-dense
> dispatch), Opus for the final whole-branch code review, Sonnet for
> mechanical per-op test/impl loops. See "Auto-memory carry-over" in
> this kickoff.
>
> **Mines to brief the Slice 7 agent:** see "Known mines laid in
> Slice 6" in this kickoff. Key ones: `.cast_matrix_type("integer",
> dgCMatrix)` dense-coerces; `.matrix_type_ok` missing character case
> (fires if any op returns a character matrix); `copy_all` axis
> collision is lazy.
>
> **Julia DAF state at handoff:** `~/src/DataAxesFormats.jl` at
> `49fbba140437387a378217c2fa658d4231d0c8c1` (unchanged since Slice 3).
> `~/src/TanayLabUtilities.jl` at `48a4a57`. Both registered as Julia
> `dev` packages in `dafr-mcview`. Before regenerating any fixture,
> check whether DAF.jl has moved.

## Status at session end

- Local `~/src/dafr-native/`: `main` at `e38c53a`, tags `slice-5` and
  `slice-6` pushed. In sync with `origin/main`.
- Local `~/src/dafr-native/dev/`: `main` at `a04b1d1` (Slice 6 exit
  note committed; Slice 7 kickoff about to land as a new commit).
- Julia repos unchanged since Slice 4 kickoff (four slices stable).
- L2 upstream PR: **declined permanently** per durable user feedback;
  no further re-ask.
