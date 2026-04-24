# Slice 10 — Wrapper parity + release polish (kickoff)

**Date:** 2026-04-23.
**Predecessor:** Slice 9d-N (tag `slice-9d-n` on `main`, exit gate at
`dev/notes/slice-9d-n-exit.md`).
**Analysis doc:** `dev/notes/2026-04-22-slice-10-wrapper-parity-analysis.md`
— full parity inventory vs the Julia-facade wrapper at `~/src/dafr`.
**Goal:** ship `dafr` 0.1.0 — first public release — with
wrapper-parity surface + documentation polish. This is the slice that
lets downstream code move off the Julia-facade wrapper onto native.
**Structure:** 4 sub-slices, ordered 10c → 10a → 10b → 10d, each with
its own kickoff/design/plan/exit cycle.

## Scope

Native-R `dafr` already has full behavioural coverage of the DAF data
model (slices 0–7 correctness + 8–9d perf parity). The gap vs the
Julia-facade wrapper is user-facing surface and release plumbing, not
data-model features. Slice 10 closes that gap and cuts a 0.1.0 tag.

The analysis doc (§1–§11) is the detailed inventory; this kickoff locks
the decisions and ordering.

## Locked decisions

| # | Topic | Decision |
|---|---|---|
| 1 | Sub-slice order | 10c → 10a → 10b → 10d |
| 2 | Query-builder object | S7 class `DafrQuery` with properties `canonical` (character) and `ast` (list). Builders compute both at construction; pipe-compose concatenates both. `get_query`/`has_query` dispatch on `DafrQuery` (eval AST directly — no re-parse) or accept a string (parse via `parse_query` then eval). `print`/`as.character`/`canonical_query` surface the canonical string for debuggability. |
| 3 | `h5df` HDF5-backed Daf store | **Deferred** to a post-release backend slice. |
| 4 | HDF5 binding for h5ad | `hdf5r`, `Suggests`-gated, only touched in 10b. |
| 5 | `tidyr`/`tibble` for `get_tidy` | `Suggests` + `rlang::check_installed` gate at the entry point. |
| 6 | Contract-function name conflicts | Keep native's `contract_scalar`/`contract_vector`/`contract_matrix` record-constructor semantics. NEWS entry documents the breakage vs wrapper; no compat shim. |
| 7 | `DafAnnData` class system | R6, read-only facade. Matches wrapper and Julia semantics — both are read-oriented bridges; writes on the facade do not flow back into the Daf. Drop to `daf_ann$daf` and use `set_*` for modifications. |
| 8 | `NEWS.md` format for 0.1.0 | Public `# dafr 0.1.0` entry at top with user-visible feature bullets; existing slice ledger folded under a `## Development history` header. |
| 9 | Version bump | `0.0.0.9000` → `0.1.0`. Git tag `v0.1.0` on 10d merge. |

## Sub-slice breakdown

### 10c — small ports (first; ~S)

Pure R, no new hard deps, small individual changes. Ships as the
landing pad: low-risk, low-coupling, unblocks 10b's handler-constant
back-reference.

**Exports added:**

- Handler constants + shim: `ERROR_HANDLER`, `WARN_HANDLER`,
  `IGNORE_HANDLER`, `inefficient_action_handler(handler)`. Load-bearing
  for 10b (wrapper passes `unsupported_handler = WARN_HANDLER` to
  `h5ad_as_daf`). Constants are the literal strings recognised by
  `register_dafr_handler`; `inefficient_action_handler` delegates to
  `register_dafr_handler("inefficient", ...)`.
- Version counters: `axis_version_counter(daf, axis)`,
  `matrix_version_counter(daf, axis_a, axis_b, name)`,
  `vector_version_counter(daf, axis, name)`. Already computed
  internally in `R/cache.R`; export thin accessors.
- Group helpers: `group_names(daf, axis, entries_of_groups, prefix)`,
  `compact_groups(group_indices)`,
  `collect_group_members(group_indices)`. Pure R, port 1:1 from wrapper.
- DataFrame helpers: `get_dataframe(daf, axis, columns = NULL,
  cache = TRUE)`, `get_dataframe_query(daf, query, cache = TRUE)`,
  `get_tidy(daf, axis, columns = NULL, cache = TRUE, ...)`. `get_tidy`
  gated on `rlang::check_installed(c("tidyr", "tibble"))`.
- Query introspection: `query_requires_relayout(daf, query)` — AST walk
  detecting whether evaluation would transpose a matrix.
  `escape_value(s)` / `unescape_value(s)` — public aliases of
  `.escape_value` plus its inverse (round-trip identity).
- Class-surface sugar: `is_daf(x)` (inherits check), `daf_name(x)`
  (`S7::prop(x, "name")`), `read_only(daf, name = NULL)` (factory that
  wraps a `DafWriter` into a `DafReadOnly`), `complete_path(daf)`
  (exported alias of `.complete_path`).
- Contract UX: `create_contract(scalars, vectors, matrices, axes, name,
  is_relaxed)`, `contract_docs(contract)`, `axis_contract(name,
  expectation, description)`, `tensor_contract(name, axes_names,
  expectation, type, description)`, `verify_contract(contract, daf)`
  (combines `verify_input` + `verify_output`).

**Test budget:** ~120 new testthat assertions (3–5 per exported
function + handler-constant round-trip + version-counter increment
guard + group-helper semantics + escape/unescape identity).

**Key files:**

- Likely new files: `R/handlers_constants.R`, `R/version_counters.R`,
  `R/groups.R`, `R/dataframes.R`, `R/query_introspection.R`,
  `R/class_sugar.R`, `R/contract_ux.R`. Alternative: extend existing
  `R/handlers.R`, `R/cache.R`, `R/contracts.R`, `R/queries.R`.
  Decision deferred to 10c kickoff — depends on post-add file size.

**Mines:**

- `contract_scalar`/`contract_vector`/`contract_matrix` already exist
  with record-constructor semantics — do NOT change them;
  `create_contract` calls them internally.
- `get_tidy` must hard-error with an installation hint if
  `tidyr`/`tibble` are missing, never silently import or fall through.
- `escape_value` is reversible ↔ `unescape_value` ∘ `escape_value` = id
  for every legal query literal. Round-trip tests are non-negotiable.
- `register_dafr_handler` already accepts
  `"ignore"`/`"warn"`/`"error"`/function — the uppercase constants are
  aliases that map to the lowercase accepted tokens. Do not introduce a
  second registry.

**Exit criterion:** R CMD INSTALL + full test suite green;
`devtools::check()` NOTE-free on the new exports; merged to `main`
with tag `slice-10c`.

### 10a — query builders (second; ~M)

The headline ergonomic win. Port the wrapper's pipe-chain query DSL
as `DafrQuery` S7 objects that wrap the existing native AST.

**Exports added:** **53 builder functions** (analysis doc §(A) total
with the deprecated¹ sub-items subtracted: 7 element-wise + 19
reductions + 13 selection/axis (16 in the analysis-doc table minus 3
¹-marked deprecated entries) + 6 logical masks + 8 comparison). The
analysis-doc headline count of 64 is a slight over-count because it
treats the ¹-marked Selection/axis entries as in-scope; re-check at
10a kickoff but the working number is 53. Categories:

- Element-wise (7): `Abs`, `Clamp`, `Convert`, `Fraction`, `Log`,
  `Round`, `Significant`.
- Reductions (19): `Count`, `CountBy`, `GeoMean`, `GroupBy`,
  `GroupColumnsBy`, `GroupRowsBy`, `Max`, `Mean`, `Median`, `Min`,
  `Mode`, `Quantile`, `ReduceToColumn`, `ReduceToRow`, `Std`, `StdN`,
  `Sum`, `Var`, `VarN`.
- Selection / axis (13): `Axis`, `AsAxis`, `BeginMask`,
  `BeginNegatedMask`, `EndMask`, `IfMissing`, `IfNot`, `LookupMatrix`,
  `LookupScalar`, `LookupVector`, `Names`, `SquareColumnIs`,
  `SquareRowIs`.
- Logical masks (6): `AndMask`, `AndNegatedMask`, `OrMask`,
  `OrNegatedMask`, `XorMask`, `XorNegatedMask`.
- Comparison (8): `IsEqual`, `IsGreater`, `IsGreaterEqual`, `IsLess`,
  `IsLessEqual`, `IsMatch`, `IsNotEqual`, `IsNotMatch`.

¹ Deprecated, do NOT port: `And`, `AndNot`, `Or`, `OrNot`, `Xor`,
`XorNot`, `Fetch`, `Lookup`, `MaskSlice`, `SquareMaskColumn`,
`SquareMaskRow`.

**Factory layer:** `R/query_builders.R` with five factory helpers
mirroring the wrapper's `R/query_factories.R` pattern —
`.make_nullary`, `.make_string_op`, `.make_value_op`,
`.make_optional_string_op`, `.make_typed_reduction`. Each factory
takes a qop name + validation rules and returns a closure that
(a) validates args, (b) builds a single-node AST fragment via the
internal `.qop_<op>` constructor, (c) computes the fragment's canonical
string via `.canonicalise_node`, (d) composes with any
`DafrQuery` arg received via `|>` by concatenating both `ast` and
`canonical`, (e) returns a `DafrQuery`.

**`DafrQuery` class:** defined in new `R/query_class.R`. S7 class with
two properties, plus `print.DafrQuery`, `as.character.DafrQuery`,
`format.DafrQuery`, and a `length` method.

**Pipe composition note.** R's native `|>` is pure syntactic
substitution — no method dispatch. Composition happens inside each
builder: at entry, the builder detects a `DafrQuery` as its first
positional arg (the pipe target), extracts it, then appends the new
fragment. This is identical to the wrapper's `extract_query_and_value`
pattern at `query_factories.R:10`; port that helper verbatim under a
`.extract_query_and_value` name.

**`get_query`/`has_query` dispatch** (modified in `R/queries.R`):

- `DafrQuery`: evaluate `q@ast` via existing `.eval_query` (no re-parse).
- `character` (length 1): `parse_query` then `.eval_query`.
- anything else: error.

**`[.DafReader`**: accept either; same dispatch rule.

**Test budget:** ~250 new testthat assertions. Per builder:
construct-in-isolation (1), pipe-compose with a prior `DafrQuery` (1),
AST identity vs `parse_query(sprintf(...))` (1), error on bad input
(1). Plus cross-cutting: canonical-string equality under composition
order changes, reduction-with-optional-type dispatch, escape
round-trip through builders with special chars.

**Key files:**

- New: `R/query_class.R` (S7 class + methods),
  `R/query_builders.R` (factories + `.extract_query_and_value`),
  `R/query_builders_exports.R` (one-liner per export).
- Modified: `R/queries.R` (extend `[.DafReader`, `get_query`,
  `has_query` dispatch).

**Mines:**

- Do NOT port deprecated wrapper builders (listed above).
- Formula authority stays in `R/operations.R` + `R/query_eval.R`.
  Builders produce AST; they never evaluate. No reduction logic in
  this slice.
- `parse_query(canonical_query(q))@ast` must equal `q@ast` for every
  builder combination exercised in tests. Non-negotiable identity check.
- `Axis("name with spaces")` must round-trip through the escape /
  canonicalise path. `.escape_value` is correct; verify it stays that
  way after any edits.
- Builder closures must capture their qop name via `force()`, not
  late-binding. The wrapper's `force(julia_fn)` pattern (see
  `query_factories.R:7,28,44,64,82`) is load-bearing for correctness
  and must survive the port.

**Exit criterion:** 53 builders exported; 250+ new assertions green;
all existing query tests still green (parser and builders produce
equivalent ASTs); merged to `main` with tag `slice-10a`.

### 10b — AnnData facade + h5ad round-trip (third; ~M)

**Exports added:**

- `DafAnnData` — R6 class, read-only facade. Active bindings for `X`,
  `obs`, `var`, `layers`, `uns`, `obs_names`, `var_names`, `n_obs`,
  `n_vars`, `shape`. Port 1:1 from wrapper `R/anndata_facade.R:25+`
  with no semantic changes. Read-only guards use the wrapper's exact
  error string for portable error-message matching.
- `as_anndata(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs")`
  — one-shot factory returning a `DafAnnData`. Matches wrapper
  signature including auto-axis detection (`cell`/`metacell` for obs,
  `gene` for var).
- `h5ad_as_daf(path, name = NULL, mode = "r",
  unsupported_handler = WARN_HANDLER)` — load a Muon-style h5ad via
  `hdf5r::H5File`, populate a `memory_daf`.
- `daf_as_h5ad(daf, path, obs_axis = NULL, var_axis = NULL,
  x_name = "UMIs", overwrite = FALSE,
  unsupported_handler = WARN_HANDLER)` — write a Daf to h5ad via
  `hdf5r`. `overwrite = FALSE` check fires BEFORE opening for write.

**`hdf5r` gating:**

- `Suggests` in DESCRIPTION; `rlang::check_installed("hdf5r")` at the
  top of each of the four h5ad entry points (the `DafAnnData` facade
  itself has NO hdf5r dep — it's a pure in-memory adapter).
- `R6` becomes a hard `Imports` (the facade class).

**Fixtures:**

- Small fixture: `inst/extdata/small_test.h5ad` — ~10 KB, 50 obs × 20
  var. Generated once by a repo script and checked in.
- Round-trip: load the wrapper's `test-anndata_format.R` /
  `test-anndata_facade.R` fixtures if present, else capture a new
  schema from a known-good round trip.

**Test budget:** ~100 new assertions:

- Facade property round-trip vs underlying Daf (15).
- Facade read-only guards — one per active binding (9).
- `as_anndata` auto-axis detection (`cell`/`metacell`/`gene`) (5).
- `as_anndata` explicit args (5).
- h5ad write-read round-trip on the fixture — X, obs, var, layers,
  uns, obs_names, var_names (30).
- `unsupported_handler` behaviour — ignore/warn/error paths (10).
- Error paths: missing file, wrong mode, overwrite-false collision,
  bad path, malformed h5ad (25+).

**Key files:**

- New: `R/anndata_facade.R`, `R/anndata_format.R`.
- Modified: `DESCRIPTION` (+`R6` Imports, +`hdf5r` Suggests),
  `NAMESPACE`.

**Mines:**

- Facade is read-only by design — every active binding must check
  `if (!missing(value)) cli::cli_abort("DafAnnData facade is
  read-only. Use the underlying Daf object to modify data.")`. Match
  the wrapper string verbatim so error-message-matching downstream
  code keeps working.
- `h5ad_as_daf` must populate a `memory_daf`, not hold a live
  connection — `hdf5r::H5File` must be closed before returning (use
  `on.exit` or `withr::defer`).
- `daf_as_h5ad`'s `overwrite = FALSE` check BEFORE opening hdf5r for
  write (else hdf5r clobbers silently).
- h5ad datasets with `dtype = O` (object arrays) decode as lists of
  bytes in hdf5r — handle the string decoding path explicitly.
- `obs`/`var` dataframes round-trip through h5ad's `/obs`/`/var`
  groups with one dataset per column. Integer columns must preserve
  dtype (not silently widen to double). Factors must survive as
  `/obs/_index` strings with a `categories` group.

**Exit criterion:** four exports working, 100+ assertions green, h5ad
round-trip on a fixture ported from the wrapper's test suite green;
merged to `main` with tag `slice-10b`.

### 10d — docs + release polish (fourth; ~M)

The release gate.

**Vignettes:**

- `vignettes/dafr.Rmd` — Getting Started analogue. `memory_daf` /
  `files_daf`, `add_axis`, `set_scalar` / `set_vector` / `set_matrix`,
  `get_*` roundtrip, `has_*`, `print(daf)`, pipe-chain query,
  `get_dataframe`, `example_cells_daf` / `example_metacells_daf`.
- `vignettes/queries.Rmd` — Query DSL tour in string form and
  builder/pipe form. Port wrapper README examples verbatim where
  syntax matches.
- `vignettes/native-performance.Rmd` — native advantages: mmap
  readers (`mmap_dgCMatrix`, `mmap_int`, `mmap_lgl`, `mmap_real`),
  `.dafr_kernel_threshold` option, bake-off headline numbers sourced
  from `dev/benchmarks/2026-04-22-post-slice-9c/report.md` (generous
  rounding).
- `vignettes/anndata.Rmd` — h5ad round-trip example (conditional on
  10b shipping; it will have).

**pkgdown site:**

Port wrapper's `_pkgdown.yml` 1:1 as the starting point, then:

- Drop "Deprecated" section entirely (native inherits no deprecated
  exports).
- Add "S7 classes" section: `DafReader`, `DafWriter`, `MemoryDaf`,
  `FilesDaf`, `FilesDafReadOnly`, `ReadOnlyChainDaf`, `WriteChainDaf`,
  `ViewDaf`, `Contract`, `ContractDaf`.
- Add "Mmap readers" section: `mmap_dgCMatrix`, `mmap_int`,
  `mmap_lgl`, `mmap_real`.
- Add "Op registry" section: `register_eltwise`, `register_reduction`,
  `registered_eltwise`, `registered_reductions`, `get_eltwise`,
  `get_reduction`.

**README rewrite:**

Port wrapper README.Rmd and adapt:

- Installation: no Julia, native only.
- Drop "Note on Data Transfer" section (no JuliaCall copy tax).
- Add "Native advantages" section: mmap-backed reads, no-Julia install,
  OpenMP kernels, `register_eltwise` / `register_reduction` for custom
  ops.
- Keep "Query syntax" mini-tutorial — accurate for native.
- Drop or rewrite "Future plans: dplyr-like API" paragraph.

**@examples backfill:** ~25 currently skipped exports (carry-over
from 9d-M kickoff). Every public export needs a runnable
`\examples{}` block. `devtools::check()` enumerates. CRAN-strict:
use `\dontrun{}` or `\donttest{}` for anything >1s, and build test
data inline (`memory_daf("demo")` + a few lines). No examples that
depend on external fixtures.

**NEWS.md rewrite:**

- `# dafr 0.1.0` entry at top with user-visible bullets grouped
  (new exports, new features, breaking changes vs wrapper, known
  gaps including `h5df` deferral).
- Existing slice ledger folded under `## Development history` header
  below.

**CRAN-submission plumbing (stub, not submit):**

- `cran-comments.md` — stub file. No CRAN submission attempt in 10d.
- `.github/workflows/` — verify R-CMD-check + pkgdown-deploy are
  green on ubuntu + macOS + windows.

**Version bump:**

- `DESCRIPTION`: `Version: 0.0.0.9000` → `Version: 0.1.0`.
- Git tag `v0.1.0` on `main` at slice-10d merge.

**Test budget:** vignette build-check only; no new testthat.

**Mines:**

- pkgdown `reference:` section must cover every exported function.
  Missing ones are a CI failure. Add a `tools/check-pkgdown-coverage.R`
  script and run it in the CI workflow.
- `@examples` that require a Daf must construct test data inline.
  Any example taking >1s in CRAN's test runner is a NOTE; wrap those
  in `\donttest{}` or `\dontrun{}` as appropriate.
- pkgdown build must run cleanly in isolation — no environment drift
  between vignettes, no cross-vignette state.

**Exit criterion:** vignettes build clean; pkgdown site renders with
no missing-man-page warnings; `R CMD check --as-cran` NOTE-free or
NOTES documented in `cran-comments.md`; NEWS 0.1.0 entry complete;
version tagged `v0.1.0`; merged to `main` with tag `slice-10d`.

## Deliberate non-goals for slice 10

- **`h5df` HDF5-backed Daf store.** Separate backend slice; post-release.
- **`set_seed(seed)`.** Julia-only RNG hook; R users have `set.seed()`.
- **Julia lifecycle glue:** `install_daf_packages`, `load_daf_packages`,
  `setup_daf`, `setup_logger`, `use_default_julia_environment`. Not
  applicable to native.
- **In-place empty sparse/dense fill** (`get_empty_dense_*`,
  `get_empty_sparse_*`, `filled_empty_*`). Degenerate even in the
  wrapper per its own README.
- **mmap S7-ctor floor** (`mmap_open_read_{scalar,vector,matrix,axis}`
  accept-class breaches from 9c + 9d). Architectural; separate slice.
- **Long-vector (>2³¹) ALTREP.** Slice-2 inherited carry-over.
- **FilesDaf multi-writer filesystem locking.** Not a wrapper feature
  either.
- **dplyr-style verbs.** Future slice; the wrapper hand-waves at these
  too.
- **CRAN submission.** Tag 0.1.0 on GitHub; CRAN is its own follow-up
  once the benchmarks-dir NOTE and installed-size NOTE are burned down.
- **Compat shims for wrapper-semantic `contract_scalar` /
  `contract_vector` / `contract_matrix`.** NEWS note only; native
  semantics stand.
- **Write-through `DafAnnData`.** Read-only only; matches wrapper +
  Julia semantics.

## Carry-over from slice 9d-N (unchanged)

- mmap S7-ctor floor (4 accept-class breaches from 9c + 9d-M).
- Two-pass flat-storage optimisation for mode/quantile per-cell
  overhead (8 GB allocator-cache footprint at 100k rows — now has
  concrete evidence from 9d-N profiling).
- Acc-struct slimming — orthogonal constant-factor work.
- `copy_all` double-write bug — small focused fix.
- 9d-M code-review minor items (unused `using`, redundant `std::sort`).

These remain deferred and independent of slice 10.

## Future post-release work

- **Public-main / dev-branch split.** Post-0.1.0, split the repo:
  clean `main` carrying only user-facing history; a dev branch on a
  separate origin for the slice-ledger detail. The `dev/` subdirectory
  is a nested repo today — the plan is to formalise that separation.
- **`h5df` HDF5 backend slice.** Port the Julia `H5df` format onto
  `hdf5r`. Own kickoff, own perf pass; shipped as a follow-up minor
  release.
- **CRAN submission.** Depends on burning down benchmarks-dir NOTE +
  installed-size NOTE.
- **dplyr-style verbs.** Future slice; analysis doc and wrapper both
  hand-wave.
- **Native-port carry-overs** (see §"Carry-over from 9d-N"). Take
  independently once 0.1.0 is out.

## Ready-to-paste prompts for sub-slice kickoffs

### 10c kickoff prompt

> Start slice 10c of the native-R `dafr` package: small ports —
> handler constants, version counters, group helpers, DataFrame
> helpers, query introspection, class-surface sugar, contract UX.
> Parent kickoff at `dev/notes/slice-10-kickoff.md`. Analysis doc at
> `dev/notes/2026-04-22-slice-10-wrapper-parity-analysis.md`.
> Predecessor: slice 9d-N, tag `slice-9d-n`.
>
> Read the parent kickoff's §"10c — small ports" first. Then use
> `superpowers:brainstorming` to resolve the file-layout question
> (new files under `R/` vs extending existing ones). Then
> `superpowers:writing-plans` for the implementation plan, broken
> into one task per export group.
>
> **Mines:** do not change `contract_scalar`/`contract_vector`/
> `contract_matrix` semantics — `create_contract` calls them
> internally. `get_tidy` must hard-error on missing `tidyr`/`tibble`,
> never silently fall through. `escape_value`/`unescape_value`
> round-trip must be identity.

### 10a kickoff prompt

> Start slice 10a of the native-R `dafr` package: query builders.
> 53 exported functions (Abs, Axis, LookupVector, IsGreater, Max,
> …) over five factory helpers producing `DafrQuery` S7 objects.
> Parent kickoff at `dev/notes/slice-10-kickoff.md`. Analysis doc §1
> has the detailed shape. Predecessor: slice 10c, tag `slice-10c`.
>
> Read the parent kickoff's §"10a — query builders" first. Then
> `superpowers:brainstorming` to resolve the composition semantics
> (how do builders detect a prior `DafrQuery` arriving via `|>` —
> port the wrapper's `extract_query_and_value` helper). Then
> `superpowers:writing-plans`.
>
> **Mines:** do NOT port deprecated wrapper builders (`And`,
> `AndNot`, `Or`, `OrNot`, `Xor`, `XorNot`, `Fetch`, `Lookup`,
> `MaskSlice`, `SquareMaskColumn`, `SquareMaskRow`). Formula
> authority stays in `R/operations.R` + `R/query_eval.R`. Builder
> closures must `force()` their qop name.
> `parse_query(canonical_query(q))@ast == q@ast` identity must
> hold per-builder and under composition.

### 10b kickoff prompt

> Start slice 10b of the native-R `dafr` package: AnnData facade +
> h5ad round-trip. Four exports: `DafAnnData` (R6, read-only),
> `as_anndata(daf, obs_axis, var_axis, x_name)`,
> `h5ad_as_daf(path, ...)`, `daf_as_h5ad(daf, path, ...)`. Parent
> kickoff at `dev/notes/slice-10-kickoff.md`. Analysis doc §2 has the
> detailed shape. Predecessor: slice 10a, tag `slice-10a`.
>
> Read the parent kickoff's §"10b — AnnData" first. Then
> `superpowers:brainstorming` to resolve h5ad schema decisions
> (obs/var dataframe round-trip, factor encoding, uns scalar types,
> string dtype handling). Then `superpowers:writing-plans`.
>
> **Mines:** read-only facade — port wrapper guard strings verbatim.
> `hdf5r` is `Suggests` + `check_installed` gate at every h5ad entry
> point. Close H5 files before returning. `overwrite = FALSE` check
> BEFORE hdf5r open-for-write. String dtype and factor round-trip
> are load-bearing.

### 10d kickoff prompt

> Start slice 10d of the native-R `dafr` package: docs + release
> polish + 0.1.0 tag. 3–4 vignettes, pkgdown site, README rewrite,
> @examples backfill, NEWS.md rewrite, version bump, `v0.1.0` tag.
> Parent kickoff at `dev/notes/slice-10-kickoff.md`. Predecessor:
> slice 10b, tag `slice-10b`.
>
> Read the parent kickoff's §"10d — docs + release polish" first.
> Then `superpowers:brainstorming` for vignette-topic scoping (are 3
> sufficient? is the performance vignette bake-off-worthy with 9c
> numbers?). Then `superpowers:writing-plans`.
>
> **Mines:** pkgdown `reference:` must cover every export (add a CI
> guard script). `@examples` on every previously-skipped export,
> CRAN-strict (use `\dontrun{}` / `\donttest{}` for >1s runs). Do
> NOT submit to CRAN in 10d; tag GitHub release only. Version bump
> goes in the same commit as the tag.
