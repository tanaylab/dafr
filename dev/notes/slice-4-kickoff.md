# Slice 4 — Kickoff breadcrumb

**Date:** 2026-04-21.
**Predecessor:** Slice 3 (tag `slice-3` on `tanaylab/dafr@main` at
`f3bcc24`), exit gate at `dev/notes/slice-3-exit.md`.

## What changed between the end of Slice 3 and now

1. **Slice 3 landed.** 31 commits between `slice-2..slice-3` plus one
   post-tag follow-up (`af842d7`, the deferred `alutil::sad()` styler
   pass). Net: +2374 / −94 across 48 files (operations registry,
   query DSL tokeniser → parser → AST → evaluator, `get_frame()`,
   query cache, `ViewDaf` + `viewer()` + `ALL_*` wildcards + last-wins
   resolution, Julia query + view fixtures). Tag `slice-3` pushed; CI
   green on the slice-3 tag and on the styler commit.
2. **Styler pass landed post-tag.** `alutil::sad()` (=
   `styler::style_pkg(indent_by = 4); devtools::document()`) committed
   as `af842d7` on `main`. The whole codebase is now 4-space indent.
   All Slice 4 R edits should match.
3. **No side-channel work otherwise.** `slice-3-queries-views` feature
   branch merged fast-forward into `main` and deleted.
4. **L2 upstream PR re-declined.** User declined again on 2026-04-20 to
   open the spec PR against `tanaylab/DataAxesFormats.jl`. The
   resolved draft at `dev/specs/filesdaf-on-disk-spec-draft.md` stays
   ready.

## Current state (as of this writing)

- **Package repo**: `/home/aviezerl/src/dafr-native/`, branch `main`,
  clean, tracking `origin/main` at `git@github.com:tanaylab/dafr.git`
  (private) at commit `af842d7`. Tag `slice-3` pushed and CI-green.
- **Dev repo** (nested, gitignored by package repo):
  `/home/aviezerl/src/dafr-native/dev/`, branch `main`, clean, no
  remote. Contains Slice 0 + 1 + 2 + 3 plans/notes/spec and this
  breadcrumb.
- **Test status**: `testthat::test_dir("tests/testthat")` —
  **939 PASS / 0 FAIL / 0 SKIP / 1 WARN** at `slice-3` tag. The single
  warning is the pre-existing `scran::quickCluster` / `irlba::irlba`
  SVD tolerance notice in `test-altrep-downstream.R`, unchanged since
  Slice 0. The styler pass changed only whitespace, no test deltas.
- **Check status**: `devtools::check(error_on = "note")` with
  `_R_CHECK_SYSTEM_CLOCK_=0` — **0 ERROR / 0 WARNING / 0 NOTE**.
- **Build status**: `pkgbuild::compile_dll(debug=FALSE)` clean. No new
  C++ in Slice 3.

## Still open from Slice 3 (tracked, primary input to Slice 4)

From the Slice 3 exit note "Scope closed vs deferred" → **Deferred to
Slice 4**:

- **Chains + Contracts** — the original Slice 3 proposal split at user
  request 2026-04-20. Both modules now land here as the primary scope.
- **ViewDaf axis rename does not propagate to vector/matrix reads** —
  `viewer(d, axes = list(list("obs", "@ cell")))` exposes the renamed
  axis via `axis_vector()` but `get_vector(v, "obs", ...)` does not
  resolve. NEWS-flagged.
- **ViewDaf axis filter does not propagate to vector/matrix reads** —
  `viewer(d, axes = list(list("cell", "@ cell [ keep ]")))` exposes
  the filtered entries via `axis_vector()` but `get_vector(v, "cell",
  ...)` returns the full base vector. NEWS-flagged.
- **`IfNot` / `AsAxis` evaluator stubs** at
  `R/query_eval.R:228-236` — parser emits them; evaluator no-ops both.
  Slice 4 wires real chain-lookup semantics.
- **NA in mask comparators** — `entries[mask]` propagates NA to
  results; Julia drops NA silently. Decide + align or document.
- **View cache dead machinery** — per-`ViewDaf` `query` cache bucket
  is created by `viewer()` (`R/view_daf.R:115-118` via
  `new_cache_env()`) but `format_get_*` methods route through
  `get_query` on the base, which uses the base's cache. The view
  bucket is never populated. Slice 4 either fills it or removes it.

From earlier slices:

- **`@examples` roxygen blocks (Z2)** — explicitly deferred again
  on 2026-04-20. Still deferred in Slice 4 per user instruction.
- **L2 upstream PR** against `tanaylab/DataAxesFormats.jl` docs —
  declined twice. Spec draft at
  `dev/specs/filesdaf-on-disk-spec-draft.md` remains resolved and
  ready. Re-ask at the Slice 4 exit gate.

## Still open from Slice 0/1/2 (unchanged, for reference)

- **Long-vector (>2^31) ALTREP scenarios** — untested.
- **`UInt32` read arm in `.read_bin_dense`** is signed-int32 under the
  hood. Pathology only surfaces for externally written fixtures with
  oversized UInt32. Harden when a consumer hits it.
- **Multi-writer filesystem locking** on FilesDaf root. v1 single-writer
  semantics mirror Julia.
- **CSC colSums bake-off** at 100M+ nnz.
- **Transpose kernel B-vs-D decision.** No real-world transpose
  consumer yet.
- **`copy_all(src, dst)`** end-to-end — local helper at
  `tests/testthat/test-files-julia-compat.R:78-91`. Promote when a
  user needs it. Slice 4's chain `complete_chain!` analog could be the
  forcing function; we are deferring `complete_chain!` for now (see
  decision in plan).

## What Slice 4 should deliver

Per the Slice 3 exit note "Deferred to Slice 4" + the user's kickoff
prompt:

1. **`ChainDaf`**. Two concrete S7 classes (`ReadOnlyChainDaf` under
   `DafReadOnly`, `WriteChainDaf` under `DafWriter`) carrying an
   ordered vector of `DafReader`s plus, for the writer, a final
   `DafWriter`. `chain_reader(dafs, name = NULL)` and
   `chain_writer(dafs, name = NULL)` constructors. All 22 `format_*`
   generics dispatch federation: reads walk `dafs` in reverse order
   (last wins), writes go to the top writer, deletes only succeed if
   the entry exists *only* in the top writer. Construction validates
   axis-entries consistency (same length, same entries) across all
   `dafs` that share an axis name.

2. **`Contract`**. S7 class with `name`, `is_relaxed`, `axes`, `data`
   slots. `ContractExpectation` enum:
   `"RequiredInput" / "OptionalInput" / "CreatedOutput" /
   "GuaranteedOutput" / "OptionalOutput"`. `contractor(computation,
   contract, daf, name = NULL, overwrite = FALSE)` wraps a `daf` in a
   `ContractDaf` (DafWriter subclass). `verify_input(daf)` and
   `verify_output(daf)` are no-ops on plain `DafReader`s and run
   the type/existence/access checks on `ContractDaf`s.
   Global enable flag mirrors Julia's `DAF_ENFORCE_CONTRACTS`
   (env var + R option).

3. **Slice 3 follow-ups.** ViewDaf axis-rename + axis-filter
   propagation to vector/matrix reads; real `IfNot` / `AsAxis`
   semantics in the evaluator; NA-in-mask alignment with Julia; view
   cache decision (probably: remove the dead bucket — chains can
   re-establish their own cache namespace later if needed).

4. **Julia compat for chains + contracts.** Extend the existing
   conda-gated Julia env with chain + contract fixtures. Reuse the
   `tests/testthat/helper-julia.R::.have_julia_env()` gate. Static
   fixture runs always; live Julia parity runs when the env is
   available.

## Known mines laid in Slice 3 for Slice 4

(Verbatim from `dev/notes/slice-3-exit.md` "Known mines" section,
plus a few Slice-4-specific implications.)

- **Query cache key is canonical string via `.canonicalise_ast()`**.
  Views do not canonicalise into the base daf's cache key-space — a
  `chain_reader(view_daf, writer)` may produce cache key collisions
  with base-daf entries. Slice 4's chain wrapper should use its own
  namespace prefix (e.g., `"view:<n>:<canon>"`) or a separate cache
  bucket. **Implication for chains:** chain wrappers should NOT
  share a cache instance with their base dafs (each chain gets its
  own `new_cache_env()`); reads on the chain may freely populate the
  chain's own cache, but base-daf caches stay isolated.
- **Evaluator state `kind` values are a closed enum**: `"init"`,
  `"scalar"`, `"axis"`, `"two_axes"`, `"matrix"`, `"vector"`,
  `"mask"`, `"grouped_vector"`, `"grouped_matrix_rows"`,
  `"grouped_matrix_cols"`, `"names"`, `"scalar_names_ready"`,
  `"vector_names_ready"`, `"matrix_names_ready"`. Implementing
  `IfNot` / `AsAxis` real semantics will likely add at least one new
  kind (e.g., `"vector_axis"` for an `=@`-marked vector ready to
  chain-resolve). Update every handler that switches on `state$kind`
  if you add a new value.
- **`.apply_axis` is load-bearing for the first→second-axis transition**
  in the evaluator state machine. Restructuring it will break `>|` /
  `>-` axis semantics. Do not touch without a full evaluator test run.
- **`IfMissing` lookahead in `.eval_query` is specific to `IfMissing`**.
  `IfNot` and `AsAxis` are evaluator stubs in Slice 3. Both need
  lookahead or AST-rewrite handling in Slice 4 to match Julia.
  **Suggested approach:** use the same "lookahead by 1" pattern as
  `IfMissing` for `AsAxis` (which always immediately precedes a
  vector/matrix lookup); but `IfNot` may need a wider lookahead since
  it can apply to a chain of lookups.
- **Matrix cache version key uses `"rows:cols"` (colon-separated,
  axis-order dependent)**. Queries on the same data under a flipped
  orientation do not share cache invalidation. If Slice 4 exposes
  relayout through views or chains, the cache key will need to
  normalise axis order (e.g., sort alphabetically).
- **NA in mask comparators produces NA in result via `entries[mask]`**.
  Julia drops `NA` silently. This divergence is latent in Slice 3 and
  untested. Document or fix in Slice 4 before contracts rely on mask
  correctness — see follow-up F2.

## Repo conventions reinforced in Slice 3 (worth re-stating)

- **4-space indent, period.** The `alutil::sad()` styler pass landed
  on `main` post-tag (`af842d7`). All Slice 4 R edits use 4-space
  indent. If unsure, run `styler::style_file(...)` before committing.
- **S7 multi-dispatch ALWAYS needs `list(ClassA, ClassB, ...)`
  signatures**, never the bare class form.
- **Tagged AST nodes via `.qop(op, ...)`** produce lists with class
  `c(paste0("qop_", op), "qop")` — lightweight alternative to S7. Add
  new node types by following `.qop(op, ...)` + a case in
  `.QOP_DISPATCH`. Don't reach for S7 here.
- **State stack-machine pattern** (`state$kind` discriminator).
  Handlers test `identical(state$kind, "...")` before mutating.
  Adding a handler without an `identical()` guard breaks the machine
  silently.
- **Julia fixture regeneration uses an inlined JSON emitter** (not
  `JSON3.jl`) because `Manifest.toml` for the `DataAxesFormats.jl`
  project is broken in the `dafr-mcview` conda env. Self-contained
  scripts in `dev/scripts/`.
- **`format_get_*` returns values WITHOUT dimnames (plain arrays)**.
  `get_*` user-facing wrappers add names. The query evaluator relies
  on this when constructing named result vectors from mask output.
  ChainDaf's `format_get_*` MUST honour the same contract — drop
  dimnames if the underlying `format_get_*` could leak them.
- **`@include` directives are load-bearing.** Any new R file
  registering S7 methods against `format_*` generics needs
  `#' @include format_api.R` at minimum (plus the concrete class
  files if instantiating against them).
- **`.assert_name` rejects `/ \ : , \n \r \t \0`** and
  leading/trailing whitespace. Query strings may contain `:`, `/`,
  `,` — do NOT route query strings through `.assert_name`. Use a
  separate validator if you need one.
- **Internal helpers use `.` prefix** and live either in `R/utils.R`
  (cross-cutting) or in the file that owns them.
- **`sort(..., method = "radix")`** for all listing returns.
- **Native headers use `.h`, not `.hpp`** — CRAN preference.
- **Use `/bin/rm` and `/bin/cp`** — the shell has `-i` aliases.
- **No emojis** in code or docs unless explicitly asked.
- **Live Julia round-trip is gated on `.have_julia_env()`** in
  `tests/testthat/helper-julia.R`. Reuse, don't re-implement.

## Julia DAF state at handoff

- `~/src/DataAxesFormats.jl` at `49fbba1` (origin/main as of handoff).
- `~/src/TanayLabUtilities.jl` at `48a4a57`.
- Both registered as Julia `dev` packages in conda env `dafr-mcview`.
- The 17-query fixture at `tests/testthat/fixtures/julia-queries/`
  was regenerated against this state — bytes unchanged vs Slice 3's
  tagged fixture. The 6 new DAF.jl commits since slice-3
  (cache_group per-item, named-tuple params, reorder_axes stubs)
  don't perturb our query-DSL output for the existing fixture.
- Slice 4's chain/contract work may want to extend the fixture to
  exercise new DAF.jl features (e.g., add a `chain_reader` /
  `contractor` round-trip alongside the existing query + view
  fixtures) once the R-side support lands.

## Ready-to-paste prompt for the next agent

Copy-paste this when starting a Slice 4 follow-up session if work
needs to resume from a fresh context:

> Start implementing Slice 4 of the native-R `dafr` package:
> chains + contracts + Slice 3 follow-ups (ViewDaf rename/filter
> propagation, IfNot/AsAxis real semantics, NA-in-mask alignment,
> view cache decision).
>
> - Package repo: `~/src/dafr-native/` on branch `main`, tracking
>   `origin/main` at `git@github.com:tanaylab/dafr.git` (private).
>   Tag `slice-3` at `f3bcc24` marks the Slice 3 exit; `main` is
>   one commit ahead at `af842d7` (post-tag styler pass).
> - Dev repo: `~/src/dafr-native/dev/` — separate nested git repo.
> - Kickoff breadcrumb:
>   `~/src/dafr-native/dev/notes/slice-4-kickoff.md` — read this
>   first.
> - Slice 4 plan (in progress / fully executed; check status):
>   `~/src/dafr-native/dev/plans/2026-04-21-slice-4-chains-contracts.md`.
> - Slice 3 exit note: `~/src/dafr-native/dev/notes/slice-3-exit.md`.
> - Existing format API (22 S7 generics): `R/format_api.R`. Both
>   `MemoryDaf` (`R/memory_daf.R`) and `FilesDaf` (`R/files_daf_*.R`)
>   implement them end-to-end. `ViewDaf` (`R/view_daf.R`) implements
>   the read-side via query rewrites.
> - Chain / Contract classes: **do not yet exist** at slice start.
>   Slice 4's first commits scaffold them.
>
> Use `superpowers:writing-plans` first if no plan exists yet, then
> `superpowers:subagent-driven-development` to execute it with full
> two-stage review per task.

## Status at session end

- `tanaylab/dafr` (private): `main` at `af842d7`, tag `slice-3`
  pushed. CI green across linux/mac/windows R-CMD-check and
  altrep-sanity on both the tag and the styler commit.
- Local `~/src/dafr-native/`: `main` at `af842d7`, clean. Feature
  branch `slice-3-queries-views` merged fast-forward and deleted.
- Local `~/src/dafr-native/dev/`: `main` clean with Slice 3 plan +
  exit note + Julia query fixture script + this kickoff breadcrumb
  committed.
- Local `~/src/DataAxesFormats.jl`: `main` at `49fbba1`. Local
  `~/src/TanayLabUtilities.jl`: `main` at `48a4a57`. Both registered
  via `Pkg.develop` in conda env `dafr-mcview`.
- L2 upstream PR (`tanaylab/DataAxesFormats.jl` docs) stays deferred
  at user request; spec draft at
  `dev/specs/filesdaf-on-disk-spec-draft.md` remains resolved.
