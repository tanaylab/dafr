# Slice 5 — Kickoff breadcrumb

**Date:** 2026-04-21.
**Predecessor:** Slice 4 (tag `slice-4` on `tanaylab/dafr@main` at `7c57565`),
exit gate at `dev/notes/slice-4-exit.md`.

## What changed between the end of Slice 3 and now

1. **Slice 4 landed.** 28 commits on `slice-4-perf-chains-contracts`, merged
   fast-forward into `main`. Net: **+3360 / −68 across 46 files**.
   - Phase P (5 commits): perf hot-path fix (`dafr.omp_threshold` wiring,
     sparse `Log eps:1` in-place log1p, bare-default reduction routing,
     fused `kernel_log_reduce_{dense,csc}` for Log→Sum|Mean, benchmark
     harness). Headline: 30K² dgCMatrix `:: UMIs % Log eps: 1 >| Sum`
     runs **589× faster / 17 838× less peak RSS** vs the pre-wedge path.
   - Phase F (6 commits): Slice-3 follow-ups (view cache reuse, NA drop
     in masks, `IfNot`/`AsAxis` single-hop chained lookup, view axis
     rename + filter propagation to vector/matrix reads).
   - Phase C (9 commits): chains — `ReadOnlyChainDaf`/`WriteChainDaf` +
     `chain_reader()`/`chain_writer()` + full read/write dispatch
     (auto-add-axis, delete-earlier-error) + Julia fixture round-trip.
   - Phase T (6 commits): contracts — `Contract` + 5 expectation
     constants + 3 builders; `ContractDaf` + `contractor()` + enforcement
     gate (`DAF_ENFORCE_CONTRACTS` / `options(dafr.enforce_contracts)`);
     22 format_* methods + access tracking + relaxed mode;
     `verify_input`/`verify_output`; `merge_contracts` (Julia `|>` port);
     end-to-end tests on MemoryDaf + FilesDaf.
   - Phase Z (1 commit): docs regeneration; 0/0/0 on `devtools::check()`.

2. **`main` pushed, tag `slice-4` pushed.** `git push origin main --tags`
   completed 2026-04-21. Feature branch `slice-4-perf-chains-contracts`
   deleted locally.

3. **No side-channel work otherwise.**

4. **L2 upstream PR status unchanged.** Still deferred per user preference.
   The resolved spec draft at `dev/specs/filesdaf-on-disk-spec-draft.md`
   stays ready.

## Current state (as of this writing)

- **Package repo**: `/home/aviezerl/src/dafr-native/`, branch `main`,
  clean, tracking `origin/main` at `git@github.com:tanaylab/dafr.git`
  (private) at commit `7c57565`. Tag `slice-4` pushed. (CI status: check
  after push propagates; Slice 3 and prior slices were all CI-green on tag.)
- **Dev repo** (nested, gitignored by package repo):
  `/home/aviezerl/src/dafr-native/dev/`, branch `main` at `c461755`
  (exit note commit). Pre-existing uncommitted/untracked files
  (`benchmarks/bake-off-results.csv` modified; `.a5c/`, slice-0 baseline
  CSVs, `notes/slice-4-kickoff.md`, `plans/2026-04-21-slice-4-chains-contracts.md`
  untracked) remain untouched — these predate Slice 4 and are outside
  this slice's scope.
- **Test status**: `testthat::test_dir("tests/testthat")` —
  **1055 PASS / 0 FAIL / 0 SKIP / 1 WARN** at `slice-4` tag. The WARN is
  the pre-existing `scran::quickCluster` / `irlba::irlba` SVD tolerance
  notice in `test-altrep-downstream.R`, unchanged since Slice 0.
- **Check status**: `devtools::check(error_on = "note")` with
  `_R_CHECK_SYSTEM_CLOCK_=0` — **0 ERROR / 0 WARNING / 0 NOTE**.
- **Build status**: `pkgbuild::compile_dll(debug = FALSE)` clean. One new
  C++ file landed in Slice 4 (`src/kernel_log_reduce.cpp`).

## Overall progress map

The design spec at `dev/specs/2026-04-19-native-r-dafr-design.md` §15 lists
8 slices for a full native-R DAF port. In practice the ordering shifted
(Slice 3 delivered more than planned; Slice 4 bundled perf + chains +
contracts). Re-mapped to delivered modules:

| Original slice goal | Status |
|---|---|
| Slice 0: scaffold, ALTREP POC, FilesDaf spec draft | **DONE** (tag `slice-0`) |
| Slice 1: `MemoryDaf` + scalar/vector/matrix + axes + cache | **DONE** (tag `slice-1`) |
| Slice 2: `FilesDaf` + mmap + Julia compat | **DONE** (tag `slice-2`) |
| Slice 3: query tokenizer/parser/AST/evaluator + eltwise/reductions | **DONE** (tag `slice-3`, merged original-Slice-4 query DSL into it) |
| Slice 4: full query DSL + QueryData cache | **DONE** (merged into Slice 3 tag) |
| Slice 5: Views + Chains + ReadOnly + axis aliasing | **DONE** (Views in `slice-3`, Chains in `slice-4`) |
| Slice 6: Contracts + Computations + Adapters | **PARTIAL** — Contracts in `slice-4`; Computations + Adapters still open |
| Slice 7: Concat + Reconstruction + Copies + Complete + Groups + ExampleData | **PARTIAL** — Groups landed in Slice 3 (query DSL `@group` etc.); rest open |
| Slice 8 (deferred): AnnData interop + Zarr backend | Deferred |

Roughly **70-75% of the design spec's core surface is delivered**. What
remains is largely "tail" functionality (higher-order helpers + bulk
manipulation) rather than foundational data model work. Put differently:
a user today can open/create a `MemoryDaf` or `FilesDaf`, run query-DSL
queries, wrap in `viewer()` / `chain_reader()` / `chain_writer()`, and
enforce contracts — end-to-end Julia parity on the core. What's missing
is: (a) ergonomic wrappers (`computation()`, `with_adapter()`), (b) bulk
copy/concat/reconstruction helpers, (c) additional op coverage beyond
the 10 defaults, (d) synthetic example datasets, (e) niche integrations.

## Still open from Slice 4 (tracked, primary input to Slice 5)

From `dev/notes/slice-4-exit.md` → **Deferred to Slice 5+**:

**Plausible Slice 5 primary scope (pick one, user decides):**

- **Option A: Computations + Adapters + ExampleData** (recommended).
  Completes the contracts user-facing story. ~740 plan lines estimated:
  - `computation(contract, fn)` HOF that wraps a function so
    `verify_input(daf)` / `verify_output(daf)` fire automatically around
    each call.
  - `with_adapter(daf, axes = c(obs = "cell", var = "gene"), fn = ...)`
    constructs a renaming `viewer()` on the fly, runs `fn`, projects
    outputs back under the original names.
  - `example_cells_daf()` / `example_metacells_daf()` synthetic datasets
    matching Julia's `ExampleData` module byte-for-byte. Enables doc
    examples + a second axis of Julia compat testing.
  - Julia-compat fixture for a computation+adapter roundtrip.
- **Option B: Tail helpers** (Copies + Concat + Complete + Reconstruction).
  ~2830 plan lines, heavier. Provides bulk-data-manipulation verbs. Would
  depend on Computations + Adapters for `copies`' contract-driven path.
  Better after Option A.
- **Option C: Ops expansion**. Add the remaining ~20 Julia ops
  (`Clamp`, `Convert`, `Fraction`, `Significant`, `Type`,
  `GeoMean`, `Median`, `Quantile`, `Std`, `StdN`, `Var`, `VarN`, `All`,
  `Any`, etc.). Mostly boilerplate — one method per op × input type.
  ~1500 R + 2400 C++ LoC. Good for a perf-focused slice.
- **Option D: Multi-hop chained lookup**. Slice 4's `AsAxis` only covers
  single-hop `@ A : v =@ : w`. Julia parity requires multi-hop
  `@ A : v =@ : w =@ : u`. Smaller slice; probably pairs well with
  Option A as an evaluator follow-up.

**Smaller leftovers (nice-to-haves, not slice-worthy on their own):**

- `@examples` roxygen blocks on all exported functions — deferred since
  Slice 3. Could be a 1-session polish slice.
- Tensor keys in contracts (Julia UNTESTED path).
- `complete_chain!` disk-chain helper — defer until a consumer needs it.
- Per-thread accumulator buffers in `kernel_log_reduce_csc_cpp` row-axis
  variant — lift if profiling shows it pays.
- Long-vector (>2³¹) ALTREP scenarios — untested since Slice 0.
- UInt32 > 2³¹ read arm — inherited from Slice 2.
- Multi-writer filesystem locking on FilesDaf — inherited from Slice 2.

**L2 upstream PR** (docs for `filesdaf-on-disk-spec-draft.md` to
`tanaylab/DataAxesFormats.jl`) — declined three times; reaffirmed deferred
at Slice 4 exit. Re-ask at next slice's exit if still relevant.

## Known mines laid in Slice 4 for Slice 5

(Full list in `dev/notes/slice-4-exit.md` → "Known mines laid in Slice 4
for Slice 5". Key ones to keep in mind):

- `ContractDaf` shares its base daf's cache env — cache coherence is
  preserved, cache isolation is not. Three-layer stacks
  (`contractor -> chain_writer -> memory_daf`) all see the same cache.
- `chain_writer` auto-adds axes from earlier readers on first
  vector/matrix write. Modification-behind-the-back of earlier dafs after
  construction can leave the writer's auto-added copy stale.
- `.type_ok` / `.vector_type_ok` / `.matrix_type_ok` use R class-name
  matching only. `character` is missing from `.matrix_type_ok`'s switch
  (character matrices fall through to `inherits(m, "character")` →
  FALSE). Non-blocking; worth patching when ops land.
- `merge_contracts` type-order is
  `c("logical", "integer", "double", "numeric", "character")`. Narrower
  wins; no cross-axis moves (`integer | character` errors).
- `Contract` validator error paths mix `stop()` and `return(string)` —
  two different error prefixes depending on which field of a bad entry
  fails. Cosmetic; flagged.
- `Contract` accepts unnamed/partial-named `axes` silently. Worth adding
  an up-front guard when the first caller hits it.
- Evaluator `state$kind` is a closed enum; F4 extended it to include
  `"vector_axis"`. Any new query-DSL handler in Slice 5 must respect the
  extended set.

## Repo conventions (reinforced across Slices 0-4)

- **4-space R indent** (post-Slice-3 styler pass).
- **S7 multi-dispatch always uses `list(ClassA, ...)` signatures.**
- **`#' @include` directives are load-bearing** for S7 method registration.
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
- **Error messages mirror Julia DAF's wording** for cross-runtime parity
  where applicable.
- **Julia fixture scripts use an inline minimal JSON emitter** (Slice-3
  + Slice-4 precedent) — `JSON` isn't in `DataAxesFormats.jl`'s project
  manifest.
- **User global CLAUDE.md** says no sycophancy, contradict when needed.
- **Auto-memory: Slice-4 P3 TDD divergence note** recorded in
  `feedback_slice4_p3_tdd.md` — if Slice 5 ops work hits a similar
  TDD-passes-pre-implementation situation, decide case-by-case per that
  precedent.

## Julia DAF state at Slice 4 exit

- `~/src/DataAxesFormats.jl` at `49fbba1` (origin/main as of Slice 3
  handoff; unchanged during Slice 4 session).
- `~/src/TanayLabUtilities.jl` at `48a4a57`.
- Both registered as Julia `dev` packages in conda env `dafr-mcview`
  (Julia 1.12.5).
- Two fixture sets live in the package repo:
  - `tests/testthat/fixtures/julia-queries/` (17 queries, Slice 3).
  - `tests/testthat/fixtures/julia-chains/` (chain round-trip, Slice 4).
- A Slice 5 that lands in Computations + Adapters should probably extend
  the fixture pattern to cover a `with_adapter` round-trip.
- Before regenerating either fixture, `git pull` `DataAxesFormats.jl`
  and re-`dev` in `dafr-mcview` so byte-equivalence is meaningful.

## Ready-to-paste prompt for Slice 5

> Start implementing Slice 5 of the native-R `dafr` package.
>
> - Package repo: `~/src/dafr-native/` on branch `main`, tag `slice-4`
>   marks the Slice 4 exit at commit `7c57565`.
> - Dev repo: `~/src/dafr-native/dev/` — separate nested git repo.
> - Kickoff breadcrumb: `~/src/dafr-native/dev/notes/slice-5-kickoff.md`
>   (this document).
> - Slice 4 plan (fully executed):
>   `~/src/dafr-native/dev/plans/2026-04-21-slice-4-chains-contracts.md`.
> - Slice 4 exit note: `~/src/dafr-native/dev/notes/slice-4-exit.md`.
>
> **First step:** ask the user to pick Slice 5 scope from options A/B/C/D
> listed in the kickoff under "Plausible Slice 5 primary scope". Default
> recommendation is **Option A: Computations + Adapters + ExampleData +
> Julia-compat fixture** — completes the contracts user-facing story and
> is the narrowest win that unlocks meaningful user code.
>
> Once scope is locked in, use `superpowers:writing-plans` to draft the
> Slice 5 plan, then `superpowers:subagent-driven-development` to execute
> it with full two-stage review per task.
>
> **Known mines to brief the Slice 5 agent:** see "Known mines" section
> of `dev/notes/slice-4-exit.md`. Key ones: ContractDaf cache sharing,
> `.type_ok` limitations, `merge_contracts` type lattice coarseness,
> evaluator `state$kind` closed enum.
>
> **Julia DAF state at handoff:** `~/src/DataAxesFormats.jl` at `49fbba1`,
> `~/src/TanayLabUtilities.jl` at `48a4a57`. Both registered as Julia
> `dev` packages in `dafr-mcview`. Before regenerating fixtures, check
> whether DAF.jl has advanced and pull if so.

## Status at session end

- Local `~/src/dafr-native/`: `main` at `7c57565`, tag `slice-4` applied
  and pushed. Feature branch `slice-4-perf-chains-contracts` deleted.
- Local `~/src/dafr-native/dev/`: `main` at `c461755` with Slice 4 exit
  note committed. Pre-existing uncommitted/untracked files outside this
  slice's scope untouched.
- Remote: `origin/main` at `7c57565`, tag `slice-4` present. CI will run
  on the tag + main push (verify in next session or check GitHub).
- Julia repos unchanged since Slice 4 kickoff.
- L2 upstream PR: stays deferred; spec draft remains resolved.
