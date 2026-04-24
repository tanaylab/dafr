# Slice 3 — Kickoff breadcrumb

**Date:** 2026-04-20.
**Predecessor:** Slice 2 (tag `slice-2` on `tanaylab/dafr@main` at
`96c3bdd`), exit gate at `dev/notes/slice-2-exit.md`.

## What changed between the end of Slice 2 and now

1. **Slice 2 landed.** 34 commits between `slice-1..slice-2` plus one
   CI-green follow-up (`96c3bdd`). Net: +2664 / −201 across 70 files.
   FilesDaf backend (read + write, dense + sparse, mmap + fallback),
   mapped-tier cache integration, bidirectional Julia round-trip (via
   conda env + committed fixture), Phase-A drive-by cleanups, and the
   on-disk spec draft fully resolved. Tag `slice-2` pushed; CI green
   across linux/mac/windows R-CMD-check and altrep-sanity.
2. **No side-channel work.** `slice-2-files-daf` feature branch merged
   fast-forward into `main` and deleted.
3. **L2 upstream PR skipped.** User declined to open the spec PR against
   `tanaylab/DataAxesFormats.jl` at the Slice 2 exit. The resolved
   draft at `dev/specs/filesdaf-on-disk-spec-draft.md` remains ready to
   copy into the Julia docs tree whenever a future slice reopens it.

## Current state (as of this writing)

- **Package repo**: `/home/aviezerl/src/dafr-native/`, branch `main`,
  clean, tracking `origin/main` at `git@github.com:tanaylab/dafr.git`
  (private) at commit `96c3bdd`. Tag `slice-2` pushed and CI-green.
- **Dev repo** (nested, gitignored by package repo):
  `/home/aviezerl/src/dafr-native/dev/`, branch `main`, clean, no
  remote. Contains Slice 0 + 1 + 2 plans/notes/spec and this breadcrumb.
- **Test status**: `testthat::test_dir("tests/testthat")` —
  **707 PASS / 0 FAIL / 0 SKIP / 1 WARN**. The warning is the
  pre-existing `scran::quickCluster` / `irlba::irlba` SVD tolerance
  notice in `test-altrep-downstream.R`, unchanged since Slice 0.
- **Check status**: `devtools::check(error_on = "note")` with
  `_R_CHECK_SYSTEM_CLOCK_=0` — **0 ERROR / 0 WARNING / 0 NOTE** locally
  and on all three CI OSes (linux + mac + windows).
- **Build status**: `pkgbuild::compile_dll(debug=FALSE)` clean. C++
  sources unchanged in Slice 2 apart from what was already in place from
  Slice 0/1.

## Still open from Slice 2 (tracked, non-blocking for Slice 3)

From the Slice 2 exit note "Scope closed vs deferred" → **Deferred to
Slice 3+**:

- **L2 upstream PR** against `tanaylab/DataAxesFormats.jl` docs. Spec
  draft resolved; copy into the Julia docs tree (`docs/src/file_specs/filesdaf-on-disk.md`)
  is the remaining step. Ask the user at Slice 3 kickoff whether to
  revisit now or keep deferred.
- **`@family` / top-level package roxygen navigation.** `?memory_daf`
  doesn't link forward to `?add_axis` etc. Cheap and navigable.
- **`dafr.omp_threshold` wiring into C++ kernels.** Declared in
  `R/options.R:5`, unused. Both `kernel_eltwise_log_add.cpp` and
  `kernel_csc_colsums.cpp` use hardcoded thresholds (10000 / 1000).
  Either wire up or rename to per-kernel constants.
- **Long-vector (>2^31) and file-truncated-while-live ALTREP scenarios.**
  Untested. FilesDaf's mmap path now lights these up; add coverage when
  a consumer hits the limits.
- **`copy_all(src, dst)`** end-to-end — currently a local test helper at
  `tests/testthat/test-files-julia-compat.R:78-91`. Promote to
  package API when Slice 3's query layer needs it.
- **Multi-writer filesystem locking** on FilesDaf root. v1 single-writer
  semantics mirror Julia; revisit only if a real consumer hits it.
- **`UInt32` read arm in `.read_bin_dense` is signed-int32 under the
  hood.** Values ≥ 2^31 come back negative. `.indtype_for_size` routes
  oversized axes to UInt64 so the pathology only surfaces for externally
  written fixtures with oversized UInt32. Harden when a consumer hits it.

## Still open from Slice 0/1 (unchanged, for reference)

- **CSC colSums bake-off** at 100M+ nnz (needs a dataset larger than
  SMALL=1). Scratch script at `dev/benchmarks/run-bakeoff.R`.
- **Transpose kernel B-vs-D decision.** No real-world transpose consumer
  yet.

## What Slice 3 should deliver

Per the top-level plan `dev/plans/2026-04-19-slice-0-scaffold-and-poc.md`
§Decomposition and the Slice 2 exit note: *Slice 3 — query DSL + views +
chains + contracts*. The read-side composition layer over both MemoryDaf
and FilesDaf.

Concretely, Slice 3 likely needs to:

1. **Query DSL.** Port Julia's `Daf.Queries` parser + AST over the
   S7 `DafReader` surface. Julia's grammar uses a left-to-right
   pipeline (`/cell : UMIs % log(eps: 1)`); replicate in R. Parse
   once, cache canonicalised query string as a key into the `query`
   cache tier (already scaffolded in Slice 1).
2. **Views.** A `ViewDaf(base_daf, query)` lazy wrapper that dispatches
   `format_has_*` / `format_get_*` through the query against the base
   daf. Inherits backend characteristics (ALTREP mmap from FilesDaf,
   in-memory from MemoryDaf) — views are composition, not copy.
3. **Chains.** `ChainDaf(daf1, daf2, ..., policy)` for same-axis
   federated reads (e.g., metadata in MemoryDaf + counts in FilesDaf).
   Policy decides which backend wins on naming collisions.
4. **Contracts.** Typed pre/post-conditions for computations that
   consume a daf. Declares required axes / vectors / matrices +
   their eltypes; validated at contract check-in.
5. **Port the Julia `DafJuliaWrapper` Contracts / Views / Chains tests**
   once the above lands.
6. **Decide about `copy_all(src, dst)`** — if Slice 3 needs it to
   materialise a view into a concrete store, promote the test-local
   helper to a package API.

## Known mines laid in Slice 2 for Slice 3

- **FilesDaf's `mapped` cache tier stores ALTREP views, not
  materialised vectors.** A view/query layer that calls `get_vector`
  and then feeds into e.g. `scran::quickCluster` must either accept
  ALTREP input (most modern R C code does) or materialise via
  `as.double()`/`as.matrix()`. Don't silently re-mmap.
- **`format_get_matrix` cache key uses the STORED orientation.**
  `get_matrix(d, "cell", "gene", ...)` (primary) and `get_matrix(d,
  "gene", "cell", ...)` (flipped via relayout) currently share the
  same cache entry keyed by the primary orientation. A view that
  transposes via relayout keeps this contract.
- **`@include` directives are load-bearing.** Any new R file
  registering S7 methods against `format_*` generics needs
  `#' @include format_api.R` at minimum (plus the concrete class files
  if instantiating against `FilesDaf` / `MemoryDaf`).
- **Slice 2 added `.validate_vector_value` and `.assert_scalar_value`
  to `R/utils.R`.** Any new writer-class methods Slice 3 adds should
  reuse these, not re-implement.
- **`.read_bin_dense` / `.write_bin_dense` are the only binary I/O
  surface.** Any format additions (e.g., a `Date` scalar type) must go
  through `.dtype_canonical` + `.dtype_size` + `.dtype_for_r_vector` —
  don't sprinkle `writeBin` directly in new code.
- **`.assert_name`** now rejects `/ \ : , \n \r \t \0` and leading/
  trailing whitespace. Query strings may contain `:`, `/`, `,` — the
  query parser must NOT go through `.assert_name`. Use a separate
  validator for query strings.
- **Live Julia round-trip is gated on `.have_julia_env()`** in
  `tests/testthat/helper-julia.R`. The gate uses `Sys.which("conda")`
  + `tryCatch`, so CI without conda skips cleanly. Slice 3 tests that
  exercise the same env should reuse the same helper, not re-implement.

## Repo conventions worth re-stating

- **S7 multi-dispatch needs `list(...)` signatures.** Bare-class form
  only works for single-dispatch generics.
- **Internal helpers use `.` prefix** and live either in `R/utils.R`
  (cross-cutting, e.g., `.assert_name`) or in the backend file that
  owns them (e.g., `.files_get_vector_impl` in `R/files_daf_read.R`).
- **`sort(..., method = "radix")`** for all listing returns to keep
  results locale-independent across platforms.
- **Native headers use `.h`, not `.hpp`** — CRAN preference.
- **`src/init.cpp` does NOT exist and must NOT exist.** cpp11 owns
  `R_init_dafr`. Subsystems that need init-time registration use a
  `[[cpp11::init]]`-decorated helper.
- **Dev repo is a separate nested git repo** inside the package repo.
  Plan/note/spec/benchmark commits → dev repo. Source + test commits →
  package repo. Infer from file paths.
- **Use `/bin/rm` and `/bin/cp`** — the shell has `-i` aliases.
- **No emojis** in code or docs unless explicitly asked.
- **Descriptor JSON writes use `cat(sprintf(...))`, not `toJSON`**,
  to preserve Julia's byte-exact key order.
- **On-disk integer indices are 1-based** (Julia convention). R
  `dgCMatrix@p/@i` are 0-based. Convert at read (`- 1L`) and write
  (`+ 1L`).

## Ready-to-paste prompt for the next agent

Copy-paste this when starting the Slice 3 session:

> Start implementing Slice 3 of the native-R `dafr` package:
> query DSL + views + chains + contracts over both MemoryDaf and
> FilesDaf backends.
>
> - Package repo: `~/src/dafr-native/` on branch `main`, tracking
>   `origin/main` at `git@github.com:tanaylab/dafr.git` (private). Tag
>   `slice-2` at `96c3bdd` marks the Slice 2 exit (CI-green on
>   linux/mac/windows).
> - Dev repo: `~/src/dafr-native/dev/` — separate nested git repo
>   (gitignored by package repo). Plans, specs, notes, benchmarks go
>   here.
> - Kickoff breadcrumb: `~/src/dafr-native/dev/notes/slice-3-kickoff.md`
>   — read this first. It summarizes Slice 2 deliverables, open risks,
>   conventions, and proposed Slice 3 scope.
> - Slice 2 exit note: `~/src/dafr-native/dev/notes/slice-2-exit.md`.
> - Slice 2 plan (fully executed):
>   `~/src/dafr-native/dev/plans/2026-04-20-slice-2-files-daf.md`.
> - FilesDaf on-disk spec (all `[UNCLEAR]` markers resolved):
>   `~/src/dafr-native/dev/specs/filesdaf-on-disk-spec-draft.md`. Not
>   yet upstreamed — user declined the L2 PR at Slice 2 exit; ask again
>   if Slice 3 revisits it.
> - Existing format API (22 S7 generics):
>   `R/format_api.R`. Both `MemoryDaf` (`R/memory_daf.R`) and
>   `FilesDaf` (`R/files_daf_*.R`) implement them end-to-end.
> - User-facing readers/writers: `R/readers.R`, `R/writers.R`.
> - Query / view / chain / contract classes: **do not yet exist.**
>   Slice 3's first commits scaffold them.
>
> Use `superpowers:writing-plans` first to draft a Slice 3 plan against
> this breadcrumb's "What Slice 3 should deliver" checklist, then
> `superpowers:subagent-driven-development` to execute it with full
> two-stage review per task.
>
> Before writing any plan, (a) re-read the Slice 2 exit note to pick up
> where "closed vs deferred" decisions landed, (b) read the Julia
> `DataAxesFormats.jl` `src/queries.jl` + `src/views.jl` +
> `src/chains.jl` + `src/contracts.jl` at least at header level to
> understand what you're porting, and (c) confirm with the user
> whether to also open the deferred L2 upstream PR in this slice.

## Status at session end

- `tanaylab/dafr` (private): `main` at `96c3bdd`, tag `slice-2` pushed.
  CI green across linux/mac/windows R-CMD-check and altrep-sanity.
- Local `~/src/dafr-native/`: `main` at `96c3bdd`, clean. Feature
  branch `slice-2-files-daf` merged fast-forward and deleted.
- Local `~/src/dafr-native/dev/`: `main` clean with Slice 2 plan + exit
  note + Julia fixture script + this kickoff breadcrumb committed.
- Pre-existing scratch benchmark CSVs in `dev/benchmarks/` still
  untracked — unchanged from Slice 0/1.
