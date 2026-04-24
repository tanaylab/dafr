# Slice 6 — Kickoff breadcrumb

**Date:** 2026-04-21.
**Predecessor:** Slice 5 (tag `slice-5` on `main` at commit `79cb372`),
exit gate at `dev/notes/slice-5-exit.md`.

## What changed between Slice 4 exit and now

1. **Slice 5 landed.** 27 commits on `slice-5-computations-adapters`,
   merged fast-forward into `main` (+8847 / −16 across 104 files,
   ~2.1 MB of that is `inst/extdata/example_data/`). Net by phase:
   - Phase D (7 commits): multi-hop chained lookup.
     `.apply_chained_lookup_vector` returns `$property` on every hop so
     a subsequent bare `=@` infers the next hop's target axis; a
     `names(pivot_values)` heuristic preserves `??` row-drops across
     hops. `@ cell : donor =@ : lab =@ : country` now resolves to
     arbitrary depth; 6 new tests.
   - Phase E (4 commits): `example_cells_daf()` /
     `example_metacells_daf()` / `example_chain_daf()` +
     byte-parity data under `inst/extdata/example_data/` (18 files
     copied verbatim from DataAxesFormats.jl); `.cast_vector` /
     `.cast_matrix` / per-file loaders replicate Julia's type-cast
     lattice. Parity confirmed against Slice-3 FilesDaf dump and the
     Julia source.
   - Phase A (2 commits): `computation(name, contract, fn)` HOF +
     `function_contract(fn)` + `contract_description(contract)`. The
     contract is bound as a function attribute; verify_input/output
     run around the wrapped call.
   - Phase B (4 commits): `adapter(daf, fn, input_axes, input_data,
     output_axes, output_data, capture, empty, relayout, overwrite,
     name)` matching Julia. Internal `.copy_view_to_daf()` with
     `new` / `replace` / `pad` axis modes. Latent Slice-3 bug in
     `R/view_daf.R` (`startsWith(NULL, ...)`) fixed alongside.
   - Phase J (3 commits across both repos): Julia-parity fixture at
     `tests/testthat/fixtures/julia-adapter/` with bit-identical
     roundtrip verified against DAF.jl at
     `49fbba140437387a378217c2fa658d4231d0c8c1`.
   - Phase Z (7 commits): **`@examples` added to all 60 exported
     functions** (user-requested scope expansion; the original plan
     covered only Slice 5's 7 new exports). NEWS + exit note +
     Collate sync. `devtools::check()` stayed at 0/0/0 throughout.

2. **`main` merged fast-forward, tag `slice-5` created.** Not pushed
   yet — user deferred `git push origin main --tags`. Re-ask at the end
   of this slice.

3. **Feature branch deleted.** `slice-5-computations-adapters` gone
   locally after merge.

## Current state (as of this writing)

- **Package repo**: `/home/aviezerl/src/dafr-native/`, branch `main`,
  clean, 27 commits ahead of `origin/main` (`slice-4` at `7c57565` →
  `slice-5` at `79cb372`). Tags locally: `slice-0` … `slice-5`. Remote
  (`git@github.com:tanaylab/dafr.git`) still at `slice-4`; `slice-5`
  not yet pushed.
- **Dev repo** (nested, gitignored by package repo):
  `/home/aviezerl/src/dafr-native/dev/`, branch `main` at `a9f4aca`
  (Slice 5 exit commit). Pre-existing untracked files from prior
  slices (benchmarks, slice-0 baselines, some notes/plans) remain
  untouched.
- **Test status**: `testthat::test_dir("tests/testthat")` —
  **1175 PASS / 0 FAIL / 0 SKIP / 1 WARN** at `slice-5`. The WARN is
  the pre-existing `scran::quickCluster` / `irlba::irlba` SVD tolerance
  notice in `test-altrep-downstream.R`, unchanged since Slice 0.
- **Check status**: `devtools::check(error_on = "note")` with
  `_R_CHECK_SYSTEM_CLOCK_=0` — **0 ERROR / 0 WARNING / 0 NOTE**.
- **Build status**: `pkgbuild::compile_dll(debug = FALSE)` clean. No
  new C++ files in Slice 5 (evaluator patch + R-only work).
- **Public surface**: 83 exports. Every exported function has
  `@examples` (60 functions — constants and S7 class aliases skipped).

## Overall progress map

Design spec at `dev/specs/2026-04-19-native-r-dafr-design.md` §15
originally listed 8 slices. Delivered surface:

| Original slice goal | Status |
|---|---|
| Slice 0: scaffold, ALTREP POC, FilesDaf spec draft | **DONE** (tag `slice-0`) |
| Slice 1: MemoryDaf + scalar/vector/matrix + axes + cache | **DONE** (tag `slice-1`) |
| Slice 2: FilesDaf + mmap + Julia compat | **DONE** (tag `slice-2`) |
| Slice 3: query tokenizer/parser/AST/evaluator + eltwise/reductions | **DONE** (tag `slice-3`, merged original-Slice-4 query DSL into it) |
| Slice 4: full query DSL + QueryData cache | **DONE** (merged into `slice-3` tag) |
| Slice 5: Views + Chains + ReadOnly + axis aliasing | **DONE** (Views in `slice-3`, Chains in `slice-4`) |
| Slice 6: Contracts + Computations + Adapters | **DONE** (Contracts in `slice-4`; Computations + Adapters in `slice-5`) |
| Slice 7: Concat + Reconstruction + Copies + Complete + Groups + ExampleData | **PARTIAL** — Groups in `slice-3`; ExampleData in `slice-5`; Copies + Concat + Reconstruction + Complete still open |
| Slice 8 (deferred): AnnData interop + Zarr backend | Deferred |

Roughly **78-82% of the design spec's core surface is delivered**.
What remains is "tail" functionality: bulk manipulation verbs,
additional op coverage beyond the current 10 defaults, niche
integrations.

A user today can: construct/open `MemoryDaf` or `FilesDaf`; run
query-DSL queries (including multi-hop chained lookups); compose
`viewer()` / `chain_reader()` / `chain_writer()`; enforce contracts
via `computation()`; adapt computations to renamed data via
`adapter()`; and start from byte-parity `example_cells_daf()` /
`example_metacells_daf()`. What's missing: bulk copy/concat, more
ops, reconstruction helpers, niche backends.

## Still open from Slice 5 (tracked, primary input to Slice 6)

From `dev/notes/slice-5-exit.md` → **Deferred to Slice 6+**:

**Plausible Slice 6 primary scope (pick one, user decides):**

- **Option B: Copies + Concat + Complete + Reconstruction + bulk
  helpers** (recommended; still the kickoff's long-standing
  recommendation). Biggest single gap in the delivered surface —
  the `copy_all!` public API, cross-daf copying with `empty` /
  `relayout` / `overwrite`, `concat` along an axis, `complete_chain`
  for on-disk chains, and reconstruction helpers (rebuild a daf from
  a sub-daf + a reorder mask). Naturally consumes `adapter()`'s
  internal `.copy_view_to_daf` as a starting point — promote it to a
  public `copy_all()` with more knobs. ~2800 plan lines estimated.
  Depends on nothing net-new; exercises existing machinery.
- **Option C: Ops expansion**. Remaining ~20 Julia ops
  (`Clamp`, `Convert`, `Fraction`, `Significant`, `Type`, `GeoMean`,
  `Median`, `Quantile`, `Std`, `StdN`, `Var`, `VarN`, `All`, `Any`,
  plus any stragglers). Mostly boilerplate — one method per op × input
  type (dense / sparse / logical). ~1500 R + 2400 C++ LoC (C++
  optional — start R-only and add C++ only where profiles justify).
  Perf-focused slice.
- **Option E: Tensor keys in contracts + type-lattice cleanup**. Add
  tensor / N-axis keys to `Contract` (Julia UNTESTED upstream but the
  design surface exists); fix the `.matrix_type_ok` missing `character`
  case; widen the `merge_contracts` type lattice if needed. Small slice
  (~300 LoC); closes the Slice-5 mines. Could bolt on to Option B.
- **Option F: AnnData interop (partial)** or **Zarr backend (partial)**.
  Originally Slice 8 of the design spec; still deferred. Only bring up
  if a user workflow strongly pulls for it.

**Smaller leftovers (nice-to-haves):**

- `@examples` for the 25 skipped exports (string constants + S7 class
  names). Both are low-value (constants don't have examples; class
  docs route through the constructor). Not worth a slice.
- Sparse-aware pad-mode matrix copy in `.copy_view_to_daf` (currently
  dense-coerces via `as.matrix(val)` — fine at `example_cells_daf`
  scale, footgun at 50k×30k). Belongs to Option B when `copy_all` is
  generalised.
- `computation()` dual-/triple-contract forms (Julia UNTESTED). Revisit
  if a user hits the limitation.
- Long-vector (>2³¹) ALTREP scenarios. Untested since Slice 0.
- UInt32 > 2³¹ read arm. Inherited from Slice 2.
- Multi-writer filesystem locking on FilesDaf. Inherited from Slice 2.

**L2 upstream PR** (docs for `filesdaf-on-disk-spec-draft.md` to
`tanaylab/DataAxesFormats.jl`) — declined four times across Slices
3/4/5. **Re-ask at Slice 6 exit if still relevant.**

## Known mines laid in Slice 5 for Slice 6

(Full list in `dev/notes/slice-5-exit.md`. Key ones to brief Slice 6):

- `.copy_view_to_daf`'s pad-mode matrix copy calls `as.matrix(val)` —
  dense-coerces any sparse input. At `example_cells_daf` scale this is
  ~5 MB; at 50k × 30k it's ~12 GB. **Option B should fix this when
  promoting to public `copy_all()`.**
- `computation()` stores the contract as a function attribute, not in
  an S7 class. Robust across normal R round-trips (tested) but not
  introspectable via `S7::prop()` / `methods::slotNames()`. Any Slice-6
  feature that needs structured contract metadata should either
  refactor to an S7 wrapper or use `function_contract()`.
- `function_contract()` errors if `dafr_contract` attr is missing; no
  `tryCatch`-friendly sentinel. If Slice 6 needs introspection-safe
  access, add a second entry point `function_contract(fn,
  fallback = NULL)`.
- `merge_contracts` type lattice
  (`c("logical", "integer", "double", "numeric", "character")`) is
  coarse; no cross-axis moves. Unchanged from Slice 4. Option E would
  address.
- `.matrix_type_ok` is missing the `character` case (pre-existing
  Slice-4 mine). Slice 5 did not exercise it — no contract in the new
  tests specifies a `character`-typed matrix. A `character`-matrix
  contract would silently fall through `inherits()` and produce a
  misleading error.
- `@examples` for `get_reduction("Sum")` / `get_eltwise("Abs")` depend
  on `.register_default_ops()` running at package load. If that
  registration is ever made lazy, the examples will break.
- Evaluator `state$kind` remains a closed enum (`init`, `axis`,
  `vector`, `vector_axis`, `matrix`, `mask`, `scalar`, `names`,
  `grouped_vector`, `grouped_matrix_rows`, `grouped_matrix_cols`).
  Any new query-DSL handler in Slice 6 must respect it.
- `empty` pad-mode only supports flat-key form (`"axis|vector"` /
  `"rows|cols|matrix"`). Julia's `EmptyData` nested structure is not
  supported. Option B (`copy_all!`) should widen this.

## Repo conventions (reinforced across Slices 0-5)

- **4-space R indent** (post-Slice-3 styler pass).
- **S7 multi-dispatch always uses `list(ClassA, ...)` signatures.**
- **`#' @include` directives are load-bearing** for S7 method
  registration. Leaf files (those without their own `@include`) are
  typically omitted from an includer's list, but Slice 5 showed this
  is not a hard rule — `example_data.R` still lists leaves
  (`classes.R`, `memory_daf.R`, `writers.R`) because trimming them
  reorders Collate. **Empirical test (`devtools::document()` leaves
  Collate byte-identical) is the ground-truth convention.**
- **`format_get_*` returns plain arrays without dimnames**;
  `get_*` user-facing wrappers add names.
- **`sort(..., method = "radix")`** for all listing returns.
- **`.dafr_builtin` attribute** on default-op functions is the identity
  hook used by P2/P3/P4.
- **Shell aliases `rm`/`cp` are interactive** — use `/bin/rm`/`/bin/cp`
  if scripting destructive operations.
- **No emojis.**
- **Never `--no-verify` / `--amend` / force-push.** Always NEW commits.
- **Native C++ headers use `.h`, not `.hpp`.**
- **Error messages mirror Julia DAF's wording** for cross-runtime
  parity where applicable.
- **Julia fixture scripts use an inline minimal JSON emitter** (Slice-3
  + Slice-4 + Slice-5 precedent) — `JSON` isn't in DAF.jl's project
  manifest.
- **`S7::prop()` outside validator bodies**, `@` only inside
  `validator = function(self) { ... }` (Slice 5 `contract_description`
  review established this).
- **`.assert_name(x, "x")`** is the project's standard name validator
  (Slice 5 established this for `computation()`).
- **User global CLAUDE.md** says no sycophancy, contradict when needed.
- **Auto-memory: Slice-4 P3 TDD divergence note** recorded in
  `feedback_slice4_p3_tdd.md`. Slice 5 re-invoked this precedent
  twice (Phase A's 1-commit bundle, Phase B's 2-commit bundle). It's
  now de facto the convention: if a bundled-commit implementation
  ends with all-tests-passing and the audit trail is preserved in
  the commit message, it's acceptable.

## Julia DAF state at Slice 5 exit

- `~/src/DataAxesFormats.jl` at
  `49fbba140437387a378217c2fa658d4231d0c8c1` (unchanged since Slice 3
  — three slices of stability).
- `~/src/TanayLabUtilities.jl` at `48a4a57`.
- Both registered as Julia `dev` packages in conda env `dafr-mcview`
  (Julia 1.12.5).
- Three fixture sets live in the package repo:
  - `tests/testthat/fixtures/julia-queries/` (17 queries, Slice 3)
  - `tests/testthat/fixtures/julia-chains/` (chain round-trip, Slice 4)
  - `tests/testthat/fixtures/julia-adapter/` (computation + adapter
    round-trip, Slice 5)
- A Slice 6 that touches `Copies` / `Concat` / `Complete` should
  extend the fixture pattern to cover a round-trip. Before
  regenerating fixtures, `git -C ~/src/DataAxesFormats.jl pull
  --ff-only` and re-`dev` in `dafr-mcview` so byte-equivalence is
  meaningful.

## Model selection guidance

**Opus is acceptable for code review.** Phases in Slice 5 used Sonnet
for both implementation and review; this was cost-driven, not
quality-driven. Two observations justify routing complex reviews
through Opus when they're stakes-heavy:

1. The final cross-phase review in Slice 5 (`superpowers:code-reviewer`
   on the full 26-commit branch) surfaced genuine cross-cutting
   concerns (Collate DAG integrity, S7 method-registration load order,
   global-state leakage in examples) — Sonnet handled it, but Opus is
   likely more penetrating on a whole-branch sweep where the reviewer
   has to hold 8000+ line diffs in context and spot systemic issues.
2. For single-file reviews on mechanical phases (Phase D, Phase E,
   Phase J regen script), Sonnet was entirely sufficient and the cost
   differential favours it.

**Heuristic:** per-phase implementer + spec review = Sonnet by default.
Final whole-branch code review and any review that surfaces cross-phase
architectural concerns = Opus is a reasonable choice. Implementer
dispatches that involve architectural design decisions (not just
pattern-matching against a detailed plan) also benefit from Opus.

Don't strictly use Sonnet when a more capable model would catch more.

## Ready-to-paste prompt for Slice 6

> Start implementing Slice 6 of the native-R `dafr` package.
>
> - Package repo: `~/src/dafr-native/` on branch `main`, tag `slice-5`
>   marks the Slice 5 exit at commit `79cb372`.
> - Dev repo: `~/src/dafr-native/dev/` — separate nested git repo.
> - Kickoff breadcrumb: `~/src/dafr-native/dev/notes/slice-6-kickoff.md`
>   (this document).
> - Slice 5 plan (fully executed):
>   `~/src/dafr-native/dev/plans/2026-04-21-slice-5-computations-adapters-multihop.md`.
> - Slice 5 exit note: `~/src/dafr-native/dev/notes/slice-5-exit.md`.
>
> **First step:** ask the user to pick Slice 6 scope from options
> B / C / E / F listed in the kickoff under "Plausible Slice 6 primary
> scope". Default recommendation is **Option B: Copies + Concat +
> Complete + Reconstruction + bulk helpers** — the biggest single gap
> in the delivered surface, and the natural consumer of Slice 5's
> internal `.copy_view_to_daf()` (promote to public `copy_all()` with
> more knobs).
>
> Once scope is locked in, use `superpowers:writing-plans` to draft the
> Slice 6 plan, then `superpowers:subagent-driven-development` to
> execute it with two-stage review per task.
>
> **Model selection:** Sonnet for per-phase implementer + spec review
> by default. Opus is a reasonable choice for the final whole-branch
> code review and for any implementer dispatch that involves real
> design judgment (as opposed to mechanical plan-following). See the
> "Model selection guidance" section of this kickoff.
>
> **Known mines to brief the Slice 6 agent:** see "Known mines" section
> of `dev/notes/slice-5-exit.md`. Key ones: `.copy_view_to_daf`
> dense-coerces sparse (Option B must fix this when generalising);
> `merge_contracts` type-lattice coarseness; `.matrix_type_ok` missing
> `character` case; evaluator `state$kind` closed enum; `empty`
> pad-mode flat-key-only.
>
> **Julia DAF state at handoff:** `~/src/DataAxesFormats.jl` at
> `49fbba140437387a378217c2fa658d4231d0c8c1` (unchanged since Slice 3),
> `~/src/TanayLabUtilities.jl` at `48a4a57`. Both registered as Julia
> `dev` packages in `dafr-mcview`. Before regenerating fixtures,
> check whether DAF.jl has advanced and pull if so.

## Status at session end

- Local `~/src/dafr-native/`: `main` at `79cb372`, tag `slice-5`
  applied. Feature branch `slice-5-computations-adapters` deleted.
  **Not pushed** — `origin/main` still at `7c57565`, no `slice-5` tag
  on origin yet. Push deferred to user discretion.
- Local `~/src/dafr-native/dev/`: `main` at `a9f4aca` (Slice 5 exit
  note committed; Slice 6 kickoff about to land as a new commit).
- Julia repos unchanged since Slice 4 kickoff.
- L2 upstream PR: stays deferred; spec draft remains resolved at
  `dev/specs/filesdaf-on-disk-spec-draft.md`.
