# Slice 10 — Wrapper-Parity + Release-Polish Analysis

**Date:** 2026-04-22
**Status:** scoping / analysis only. Slice 9d-M is in progress (kickoff at
`dev/notes/slice-9d-m-kickoff.md`); this doc describes what comes **after**
9d merges and is intended as the scoping material that the eventual
Slice 10 kickoff will draw from.
**Purpose:** compare the native-R `dafr` package (this repo) against the
Julia-facade `dafJuliaWrapper` at `~/src/dafr` (the package we are
replacing) and enumerate what is missing before we can credibly ship a
public release.

## TL;DR

Native has full behavioural coverage of the DAF data model (7 slices of
correctness plus 9a/9b/9c perf-parity work). The missing pieces are not
data-model features but **user-facing surface**:

1. **Query-builder functions** (`Axis()`, `LookupVector()`, `Max()`,
   `IsGreater()`, …) — the Julia-wrapper's biggest ergonomic feature and
   roughly **64 exported functions**. None exist in native. Users write
   `daf[Axis("cell") |> LookupVector("age") |> IsGreater(2)]` today. In
   native they can only write the string form `daf["@ cell : age > 2"]`.
2. **AnnData interop** (`h5ad_as_daf`, `daf_as_h5ad`, `DafAnnData`,
   `as_anndata`, `h5df`) — the entire h5ad/HDF5 surface. This is the
   single biggest blocker for single-cell users who want to move between
   `dafr` and `anndata`/`scanpy`/`Seurat`.
3. **Contract UX** — native exposes a lower-level
   `Contract`/`ContractDaf` class, but the Julia-wrapper's
   `create_contract()` / `contract_docs()` / `verify_contract()`
   workflow is missing. `verify_input`/`verify_output` exist;
   `verify_contract`, `create_contract`, `contract_docs` do not.
4. **Handler semantics** — native's `register_dafr_handler()` is a nice
   model, but the **`ERROR_HANDLER` / `WARN_HANDLER` / `IGNORE_HANDLER`
   constants** and the user-facing `inefficient_action_handler()` entry
   point are missing. The Julia-wrapper passes these constants into
   `h5ad_as_daf(..., unsupported_handler = WARN_HANDLER)` so they are
   load-bearing for the AnnData port anyway.
5. **Release metadata** — no vignette, no `_pkgdown.yml`, no
   `cran-comments.md`, no `environment.yml`, no `conda-recipe/`,
   README still says "pre-alpha, not yet installable". These are all
   present in the Julia-wrapper and must be there for a public tag.
6. **Tests** — native runs 1909 testthat assertions across
   ~50 files; Julia-wrapper has ~25 files but a far weaker denominator.
   The missing coverage in native is mainly **the things that are not
   yet implemented** (AnnData round-trip, query-builder pipe chains,
   contract-doc rendering).

Nothing architectural is missing. This is a parity + polish slice, not
a design slice.

## Export-level diff

Julia-wrapper NAMESPACE has **182 exports**; native has **105**. The
delta is not symmetric — native has things the wrapper doesn't (S7
classes, mmap readers, eltwise/reduction registration), and the wrapper
has things native doesn't (query builders, AnnData, handler constants,
…). Breakdown below.

### (A) In wrapper, NOT in native — the parity work

| Category | Functions | Count |
|---|---|---|
| **Query builders — element-wise** | `Abs`, `Clamp`, `Convert`, `Fraction`, `Log`, `Round`, `Significant` | 7 |
| **Query builders — reduction** | `Count`, `CountBy`, `GeoMean`, `GroupBy`, `GroupColumnsBy`, `GroupRowsBy`, `Max`, `Mean`, `Median`, `Min`, `Mode`, `Quantile`, `ReduceToColumn`, `ReduceToRow`, `Std`, `StdN`, `Sum`, `Var`, `VarN` | 19 |
| **Query builders — selection / axis** | `Axis`, `AsAxis`, `BeginMask`, `BeginNegatedMask`, `EndMask`, `IfMissing`, `IfNot`, `LookupMatrix`, `LookupScalar`, `LookupVector`, `MaskSlice`¹, `Names`, `SquareColumnIs`, `SquareMaskColumn`¹, `SquareMaskRow`¹, `SquareRowIs` | 16 |
| **Query builders — logical masks** | `AndMask`, `AndNegatedMask`, `OrMask`, `OrNegatedMask`, `XorMask`, `XorNegatedMask` | 6 |
| **Query builders — comparison** | `IsEqual`, `IsGreater`, `IsGreaterEqual`, `IsLess`, `IsLessEqual`, `IsMatch`, `IsNotEqual`, `IsNotMatch` | 8 |
| **Query builders — deprecated¹** | `And`, `AndNot`, `Fetch`, `Lookup`, `Or`, `OrNot`, `Xor`, `XorNot` | 8 |
| **AnnData / HDF5 formats** | `DafAnnData`, `as_anndata`, `daf_as_h5ad`, `h5ad_as_daf`, `h5df` | 5 |
| **Handlers** | `ERROR_HANDLER`, `IGNORE_HANDLER`, `WARN_HANDLER`, `inefficient_action_handler` | 4 |
| **Contract UX** | `axis_contract`, `contract_docs`, `create_contract`, `matrix_contract`², `scalar_contract`², `tensor_contract`, `vector_contract`², `verify_contract` | 8 |
| **Version counters** | `axis_version_counter`, `matrix_version_counter`, `vector_version_counter` | 3 |
| **Groups** | `collect_group_members`, `compact_groups`, `group_names` | 3 |
| **DataFrame helpers** | `get_dataframe`, `get_dataframe_query`, `get_tidy` | 3 |
| **Empty/filled accessors** | `filled_empty_sparse_matrix`, `filled_empty_sparse_vector`, `get_empty_dense_matrix`, `get_empty_dense_vector`, `get_empty_sparse_matrix`, `get_empty_sparse_vector` | 6 |
| **Query introspection** | `query_requires_relayout`, `escape_value`, `unescape_value` | 3 |
| **Daf class surface (S3)** | `Daf`, `is_daf`, `[.Daf`, `print.Daf`, `daf_name`, `name` (deprec), `read_only` | 7 |
| **Other** | `complete_path`, `set_seed`³ | 2 |
| **Julia-lifecycle (wrapper-only, DO NOT port)**³ | `install_daf_packages`, `load_daf_packages`, `setup_daf`, `setup_logger`, `use_default_julia_environment` | 5 |

¹ Deprecated in wrapper; do not re-implement in native.
² Name conflict: native already exports `contract_scalar` / `contract_vector` / `contract_matrix`. These are record constructors for the `Contract` class; the wrapper's versions are different (direct-to-Julia). Have to resolve — probably keep native names and add deprecated aliases.
³ Julia-specific runtime glue; no native analogue needed.

**Headline counts** (excluding deprecated¹ and Julia-only³):
- **64 query-builder functions** to port (one category — the big one).
- **~40 other functions** to port/adapt.

### (B) In native, NOT in wrapper — the extras native brings

These are deliberately kept and (where useful) exported publicly:

| Category | Functions |
|---|---|
| S7 classes | `DafReader`, `DafWriter`, `DafReadOnly`, `MemoryDaf`, `FilesDaf`, `FilesDafReadOnly`, `ReadOnlyChainDaf`, `WriteChainDaf`, `ViewDaf`, `Contract`, `ContractDaf` |
| mmap readers | `mmap_dgCMatrix`, `mmap_int`, `mmap_lgl`, `mmap_real` |
| Merge actions | `MERGE_COLLECT_AXIS`, `MERGE_LAST_VALUE`, `MERGE_SKIP` |
| Computation | `computation`, `function_contract`, `merge_contracts` |
| Query introspection | `canonical_query` |
| Op registry | `register_eltwise`, `register_reduction`, `registered_eltwise`, `registered_reductions`, `get_eltwise`, `get_reduction` |
| Handlers | `register_dafr_handler` |
| Contract | `contract_description`, `empty_data` |

None of these should be removed for parity. Several of them (e.g.
`register_eltwise`, `mmap_dgCMatrix`) are things the Julia-wrapper
cannot offer — native advantages to lean into in the README.

## Category-by-category detail

### 1. Query-builder functions (THE big-ticket item)

The Julia-wrapper lets users write:

```r
daf[Axis("cell") |> BeginMask("age") |> IsGreater(2) |> EndMask() |>
    LookupVector("type")]
```

Native today can only accept the string form:

```r
daf["@ cell [ age > 2 ] : type"]
```

Strings work, but the pipe-chain form is the documented "Use" example
throughout the wrapper's README and vignette, and is the interface that
downstream scripts in the lab use. We need to restore it.

**Implementation shape.** The wrapper builds Julia `Query` objects that
Julia then piped together. Native has no Julia side; each builder must
produce an **AST fragment** (a list of `list(op=..., ...)` records —
same shape emitted by `parse_query` at
`R/query_ast.R:1`). The pipe operator then composes two fragments by
concatenation. `get_query()`/`has_query()`/`query_result_dimensions()`
need to accept *either* a string *or* an AST list. The native query
parser already emits this AST, so the round-trip is well-defined:
`parse_query(some_string)` and
`Axis("cell") |> LookupVector("age")` should yield identical AST lists.

Concretely we need a tiny factory file (`R/query_builders.R`) with ~5
higher-order helpers — `.make_nullary_op`, `.make_string_op`,
`.make_value_op`, `.make_optional_string_op`, `.make_typed_reduction_op`
— that mirror the `query_factories.R` pattern from the wrapper. Each
builder function emits a single-node AST fragment with an attribute
marking it a `"dafr_query"` object; the pipe just does `c(x, y)`.
`[.DafReader` dispatch can then accept an AST directly without
re-parsing.

**Test cost.** Each builder gets 3–5 assertions (normal use, piped use,
`q == parse_query(sprintf(...))` identity, error on bad input). Budget
~250 new testthat assertions.

**Formula authority.** Must not change. All builders ultimately produce
an AST that the existing `.eval_query` can evaluate unchanged. This is
*purely* an ergonomic wrapper layer.

### 2. AnnData / HDF5 formats

Five exports missing:

- `h5df(path, mode, name)` — HDF5-backed Daf store. **The wrapper opens
  a Julia `H5df` object via `HDF5.jl`.** Native would need to add a new
  format backend. Two options:
  - **Port a native H5DF backend using `hdf5r`** (or `rhdf5`). Full
    round-trip parity with Julia H5df files. Biggest payoff, biggest
    scope.
  - **Defer.** Ship the release without `h5df`, tell users to use
    `files_daf` + on-disk for persistence. Honest about the gap.
  My recommendation: **defer to post-release**. H5 is a separate
  backend-slice and arguably deserves its own perf pass.
- `h5ad_as_daf(path, ...)` / `daf_as_h5ad(daf, path, ...)` — Muon-style
  h5ad round-trip. Critical for single-cell users. Can be built on top
  of `hdf5r` without requiring the full `h5df` backend: read h5ad into
  a `memory_daf`, write a `memory_daf` back to h5ad. This is a narrower
  scope than `h5df` and a must-have.
- `DafAnnData` / `as_anndata(daf, obs_axis, var_axis, x_name)` — live
  read-only facade (R6 object in wrapper) that exposes `$X`, `$obs`,
  `$var`, `$layers`, `$uns`, `$obs_names`, `$var_names`, `$n_obs`,
  `$n_vars`, `$shape`. **No h5ad file I/O required** — pure adapter
  over an in-memory Daf. Port 1:1; the native `dafr` already has
  everything it needs (`matrices_set`, `vectors_set`, `scalars_set`,
  `get_matrix`, `get_vector`, `get_scalar`, `axis_vector`,
  `axis_length`).

**Recommendation:** port `DafAnnData`/`as_anndata` and
`h5ad_as_daf`/`daf_as_h5ad`. Defer `h5df` — file it as a post-release
backend slice.

### 3. Contract UX

Native has the Contract class and data model, but several ergonomic
functions are missing:

- `create_contract(scalars, vectors, matrices, axes, name, is_relaxed)`
  — one-shot constructor that builds a `Contract` from named lists.
  Currently users have to new a `Contract` directly with `Contract()`
  and populate the `axes` and `data` slots by hand. Low effort.
- `contract_docs(contract)` — renders a contract as human-readable
  Markdown listing inputs/outputs. Currently `contract_description`
  exists but returns an R list, not a rendered string. Easy to port.
- `verify_contract(contract, daf)` — single-shot verify that a Daf
  satisfies a contract, combining `verify_input` and `verify_output`.
  Optional; `verify_input` + `verify_output` already cover it.
- `axis_contract(name, expectation, description)` — convenience
  constructor for axis records in a Contract. Currently users hand-
  write `list(expectation, description)` 2-tuples. Low effort.
- `tensor_contract(name, axes_names, expectation, type, description)`
  — tensor-contract record constructor. Only needed if `copy_tensor`
  consumers want contract-level tensor entries. `copy_tensor` exists in
  native but contracts currently only cover scalars/vectors/matrices.

**Name conflict watch.** `contract_scalar`, `contract_vector`,
`contract_matrix` already exist in native with the **record constructor**
semantics. The wrapper also exports these names but with a **different
signature** (direct Julia-call through). We keep native's semantics and
do **not** change them. If any users migrate code from the wrapper they
will need to adapt; this is acceptable because the native Contract
class is a different, better-typed object.

### 4. Handler semantics

Native has `register_dafr_handler(category, action)` where action is
one of `"ignore" | "warn" | "error" | function(...)`. The wrapper has
the **older** uppercase constants (`IGNORE_HANDLER = "IgnoreHandler"`,
etc.) and a single entry point `inefficient_action_handler(handler)`
that sets the Julia-side handler.

**Decision:** export the uppercase constants as aliases and add a
compatibility `inefficient_action_handler(handler)` that delegates to
`register_dafr_handler("inefficient", ...)`. This preserves existing
wrapper-using code without asking everyone to rewrite.

**Additionally:** the constants are load-bearing for the AnnData port —
the wrapper passes `unsupported_handler = WARN_HANDLER` to
`h5ad_as_daf`. If we ship h5ad support we need the constants anyway.

### 5. Version counters

`axis_version_counter`, `matrix_version_counter`, `vector_version_counter`.
In the wrapper these return the Julia-side `UInt32` counter as a
string (to avoid R integer-overflow). Native has equivalent counters
internally for cache invalidation (see `R/cache.R`) but they are not
exported.

Exporting them is cheap and makes user-facing cache coordination
possible (useful for downstream code that wants to cache results keyed
on property version, not just touch the builtin cache). Two exports,
~15 lines each.

### 6. Group helpers

`group_names(daf, axis, entries_of_groups, prefix)`,
`compact_groups(group_indices)`, `collect_group_members(group_indices)`.
These are used in metacell workflows. They are small, pure, and
self-contained — `compact_groups` is literally a hash-map renumber.
Port all three directly. ~50 LOC total.

### 7. DataFrame helpers

`get_dataframe(daf, axis, columns, cache)` — returns a data.frame of
vectors for an axis, with row names set to axis entries. This is the
single most-used "bridge to tidyverse" function in the lab's wrapper-
based code. Must have it.

`get_dataframe_query(daf, query, cache)` — data.frame-formatted query
result (scalar → 1×1, vector → 1-column data.frame, matrix →
data.frame with row/col names). Must have it.

`get_tidy(daf, axis, columns, cache, ...)` — pivoted long-format
tibble via `tidyr::pivot_longer`. This pulls `tibble` + `tidyr` into
the dep tree if we do the port — not free. Two sub-options:
- Port `get_tidy` and add `tibble`/`tidyr` to `Suggests` — `get_tidy`
  errors with an installation hint if they're missing (`rlang::check_installed`).
- Skip `get_tidy`; tell users to pipe `get_dataframe()` into
  `tidyr::pivot_longer()` themselves.
My recommendation: **port it with Suggests-gating**. The wrapper
README documents it as a headline feature, downstream code uses it,
and the dependency is light.

### 8. Empty / filled sparse / dense accessors

Six functions that return Julia-side empty containers for in-place
fill before an atomic store operation. In native this only makes sense
if the backend (Memory, FilesDaf, future H5df) supports it. The
wrapper's own README notes:

> However, writing data from R to Julia (e.g., via set_vector or
> set_matrix) still involves copying through JuliaCall. This means
> that get_empty_dense_vector, get_empty_sparse_vector and their
> matrix equivalents do not provide the in-place filling benefit they
> offer in Julia.

I.e., **even in the Julia-wrapper these functions are degenerate** —
they allocate in Julia and copy-in from R. For native, the benefit is
real only if we can return a pointer-writeable buffer. `MemoryDaf` can
(we control the storage). `FilesDaf` cannot unless we mmap-for-write.

**Recommendation:** **defer**. Not worth the complexity for a facility
that was admittedly not useful in the wrapper either. If a user
actually needs in-place fill, they can build a dense R matrix/vector
and call `set_matrix`/`set_vector` — same memory cost as the
wrapper's implementation today. Document the gap in NEWS.

### 9. Query-introspection helpers

- `query_requires_relayout(daf, query)` — returns `TRUE` if a query's
  efficient evaluation would require transposing a matrix. Cheap AST
  walk; ~30 LOC against the existing native parser.
- `escape_value(x)` / `unescape_value(x)` — pair of string utilities
  that escape special characters in query literals. Native already
  has `.escape_value` as an internal helper (see
  `R/query_ast.R`). Trivial to export and add a reverse.

All three: low effort, high value for anyone constructing queries
programmatically.

### 10. Class surface

- `is_daf(x)` — wrapper exports this as `inherits(x, "Daf")`. Native
  equivalent is `inherits(x, S7::S7_object)` **AND** `is(x, DafReader)`.
  Add `is_daf(x) <- function(x) inherits(x, "dafr::DafReader")` (or
  similar) as a one-liner sugar.
- `daf_name(x)` — extracts the name property. Native's
  `DafReader@name` already works; `daf_name(x)` is sugar for
  `S7::prop(x, "name")`. Add as one-liner.
- `read_only(daf, name = NULL)` — **missing.** Native has the
  `DafReadOnly` class but no **factory function** that promotes a
  `DafWriter` into a `DafReadOnly` wrapper. Users today have to
  construct one by hand. Add.
- `[.DafReader` — currently not registered as an S3 method. The
  wrapper supports `daf[query]` as sugar for `get_query(daf, query)`.
  Native supports it via an S7 method already; confirm with a test.
- `print(daf)` — native already implements `format`/`print` for
  `DafReader`; verify it matches the Julia-wrapper output format.

### 11. Misc

- `set_seed(seed)` — wrapper-only helper that seeds Julia's RNG. Not
  applicable to native (R's own `set.seed()` governs). Skip.
- `complete_path(daf)` — returns the canonical on-disk path of a
  `FilesDaf`, or `NULL` for in-memory. Native has `.complete_path` as
  internal (`R/complete.R:*`); export it.

## Documentation & release polish

Everything under this heading is load-bearing for a CRAN-ready or even
tarball-installable release. None of it is in the native repo yet.

### Vignettes

Wrapper has one vignette (`vignettes/dafJuliaWrapper.Rmd`, 165 lines)
covering "Getting Started". Native has **zero**. Minimum scope for
release:

- `vignettes/dafr.Rmd` — "Getting Started" analogue. Cover:
  `memory_daf` / `files_daf` creation, `add_axis` + `set_scalar` +
  `set_vector` + `set_matrix`, `get_*` roundtrip, `has_*` checks,
  `print(daf)`, using the pipe query form, `get_dataframe`, example
  data (`example_cells_daf`, `example_metacells_daf`).
- `vignettes/queries.Rmd` — focused tour of the query DSL in both
  string form and builder/pipe form. Pull the wrapper's README
  examples directly.
- `vignettes/native-performance.Rmd` — the native advantage. Cover:
  mmap-backed reads (`mmap_dgCMatrix`), the `.dafr_kernel_threshold`
  option, and a few bake-off numbers (generous rounding; use
  `dev/benchmarks/2026-04-22-post-slice-9c/report.md` for the
  headline numbers). This is the public-facing justification for the
  port.

If we ship h5ad (recommended), a fourth vignette `vignettes/anndata.Rmd`
covers the h5ad round-trip.

### pkgdown site

Wrapper has a full `_pkgdown.yml` that organises 170 man pages into 18
reference sections. Native has no `_pkgdown.yml`. Port the wrapper's
structure 1:1, then:
- Drop the "Deprecated" section entirely (we do not inherit deprecated
  wrapper functions).
- Add a "S7 classes" section for native extras (`DafReader`,
  `DafWriter`, `MemoryDaf`, etc.).
- Add a "Mmap readers" section for `mmap_dgCMatrix` et al.
- Add a "Op registry" section for `register_eltwise` et al.

### README

Current native README is a 13-line placeholder that says "pre-alpha,
not yet installable." This is wrong after Slice 10 ships.

Port the wrapper's README.Rmd (170 lines) verbatim and adapt:
- Change the installation instruction to reflect native (no Julia).
- Drop the "Note on Data Transfer" section (native has no copy tax).
- Add "Native advantages" section: mmap-backed reads, no-Julia
  install, OpenMP kernels.
- Keep the "Query syntax" mini-tutorial (accurate for native too).
- Drop the "Future plans: dplyr-like API" paragraph or rewrite it.

### CRAN-submission plumbing

Wrapper has `cran-comments.md`, `conda-recipe/`, `environment.yml`.
Native has none of these. For a *first* public tag we probably do
**not** target CRAN in Slice 10 (C++ footprint + Matrix dep sizing +
benchmarks directory all want shakedown). Minimum for a GitHub release
tag:
- `NEWS.md` — already present; rewrite the top to be a public 0.1.0
  entry, not an internal slice ledger. Keep the slice log below a
  `## Development history` header.
- `cran-comments.md` — stub file, even if we don't submit yet.
- `.github/workflows/` — verify R-CMD-check + pkgdown-deploy are
  green on ubuntu/macos/windows.
- `LICENSE` / `LICENSE.md` — already correct.

### Missing `@examples`

Slice 9d-M kickoff carry-over lists "~25 skipped exports" with no
runnable `@examples`. Must fix before release — CRAN policy is strict
about examples on public exports. `devtools::check()` will enumerate.

### Tests

Native: 1909 assertions across ~50 test files — strong. Gaps:
- No tests for any query-builder function (they don't exist yet).
- No tests for AnnData roundtrip (also doesn't exist yet).
- No tests for `get_dataframe` / `get_tidy`.
- No tests for `contract_docs` rendering.

Each ports naturally from the wrapper's equivalent test file
(`test-operations.R`, `test-queries.R`, `test-anndata_format.R`,
`test-anndata_facade.R`, `test-data.R` for `get_dataframe`).

## Deliberate non-goals for Slice 10

Things that could *sound* like parity work but aren't, and should not
get pulled into scope:

- **`h5df` (HDF5 backend).** Real work; own slice; post-release.
- **`set_seed`.** R has `set.seed()`; we never touched Julia's RNG.
- **`install_daf_packages` / `setup_daf` / etc.** Julia-only glue.
- **In-place empty sparse/dense fill** — noted above as degenerate
  even in the wrapper.
- **mmap S7-ctor floor** — carried from 9c/9d as a separate slice
  (`mmap_open_read_{scalar,vector,matrix,axis}` accept-class
  breaches).
- **Long-vector (>2³¹) ALTREP** — Slice-2 inherited carry-over.
- **Multi-writer filesystem locking on FilesDaf** — not a wrapper
  feature either.
- **dplyr-style verbs** — the wrapper's README hand-waves at these
  for a "future release". Native can do the same hand-wave. Not
  scope.
- **CRAN submission.** Slice 10 ships a GitHub release tag; CRAN is
  its own follow-up once we burn down the benchmarks-dir NOTE and
  the installed-size NOTE.

## Effort estimate (very rough)

| Chunk | Size | Rationale |
|---|---|---|
| Query builders (64 fns) | M | Mostly mechanical with the 5 factory helpers; bulk is tests and man pages |
| AnnData (`DafAnnData` + `as_anndata`) | S | Pure R, pure adapter over existing native API |
| h5ad I/O (`h5ad_as_daf` / `daf_as_h5ad`) | M | `hdf5r`-backed, round-trip against wrapper fixtures |
| Contract UX (`create_contract`, `contract_docs`, `verify_contract`, `axis_contract`, `tensor_contract`) | S | Thin wrappers over existing Contract class |
| Handler constants + `inefficient_action_handler` | XS | 10-line shim |
| Version counters (3 exports) | XS | Already computed internally |
| Group helpers (3 exports) | XS | Pure R, 50 LOC |
| DataFrame helpers (3 exports) | S | `get_tidy` needs Suggests-gate on `tidyr`/`tibble` |
| Query introspection (`query_requires_relayout`, `escape_value`/`unescape_value`) | XS | 30 LOC + reverse |
| Class-surface sugar (`is_daf`, `daf_name`, `read_only`, `complete_path`) | XS | One-liners |
| Vignettes (3–4 files) | M | Writing + knit-verify |
| pkgdown site | S | Port `_pkgdown.yml`; verify reference builds |
| README rewrite | XS | Port + adapt |
| `@examples` backfill (~25 exports) | S | Mostly mechanical |
| Missing tests (query builders, AnnData, contracts) | M | Port wrapper tests |

Total: one slice, Opus-heavy, roughly the same shape as 9b. Break into
sub-slices if we want to ship incrementally:

- **10a — public query builders + q-AST identity.** (M) Unblocks
  users writing pipe-chain queries; releasable on its own if we want.
- **10b — AnnData (facade + h5ad I/O) + handler constants.** (M)
  Biggest single-cell ergonomic win.
- **10c — contract UX, handlers shim, group helpers, version
  counters, data.frame helpers, query introspection, class surface.**
  (S) Grab-bag of small ports.
- **10d — docs + polish (vignettes, pkgdown, README, examples,
  missing tests, NEWS rewrite).** (M) Release gate.

Shipping order preference: **10a → 10c → 10b → 10d**. (10c is all the
small stuff that unblocks external code paths; 10b is the biggest
single dep addition — `hdf5r` — so we want the small stuff green
before we start on it.)

## Open questions to lock at Slice 10 kickoff

1. **Query-builder AST identity.** Should
   `Axis("cell") |> LookupVector("age")` produce a `"dafr_query"`-
   classed AST list, or a new S7 class `DafrQuery`? S7 is tidier but
   adds a second dispatch path to `get_query`/`has_query`. My lean:
   plain list with a class attribute — follows the wrapper's shape
   and keeps the parser authoritative.
2. **`h5df` scope.** In Slice 10 or deferred? My lean: deferred;
   document as post-release.
3. **`hdf5r` vs `rhdf5`.** For h5ad I/O. `hdf5r` is more active.
   Benchmark both against a 10-GB h5ad.
4. **`tidyr`/`tibble` dependency.** `Suggests` + gated, or hard
   `Imports`? My lean: `Suggests` + `rlang::check_installed` gate.
5. **Contract-function name conflicts.** `contract_scalar` /
   `contract_vector` / `contract_matrix` exist in both but with
   different signatures. Keep native's. Deprecated wrapper semantics
   get a one-line migration note in NEWS, not a compat shim.
6. **Exporting `DafAnnData` as an R6 class vs an S7 class.** The
   wrapper uses R6. Native is mostly S7. Mixed dispatch is fine but
   adds a dep (`R6`). My lean: match the wrapper and use R6 —
   zero-cost to add, and downstream code porting from the wrapper
   works verbatim.
7. **NEWS rewrite**: do we collapse the 9 dev-slice entries into a
   single "0.1.0 initial release" line, or keep the slice ledger
   visible? My lean: keep it but below a `## Development history`
   fold, because the slice log is genuinely useful internally.
8. **Version bump.** Currently `0.0.0.9000`. Tag this release as
   `0.1.0` once Slice 10 closes.

## Ready-to-paste prompt for Slice 10 kickoff (after 9d-M exits)

> Start scoping Slice 10 of the native-R `dafr` package. Goal:
> wrapper parity + release polish before tagging 0.1.0.
>
> - Package repo: `~/src/dafr-native`, main at whatever 9d-M exit
>   commit.
> - Dev repo: `~/src/dafr-native/dev`, separate nested git repo.
> - Analysis doc: `dev/notes/2026-04-22-slice-10-wrapper-parity-analysis.md`
>   (this document). Read it in full before writing the kickoff.
> - Reference package: `~/src/dafr` (Julia-facade wrapper), the
>   package this native port is replacing. NAMESPACE has 182 exports;
>   native has 105; the delta is the scope of this slice.
>
> Start with `superpowers:brainstorming` to lock the sub-slice
> breakdown (proposed: 10a query builders → 10c small ports →
> 10b AnnData → 10d docs/polish), then write a per-sub-slice plan
> with `superpowers:writing-plans`.
>
> **Mines:**
> - Do NOT change formula authority in `R/operations.R` or the query
>   AST shape emitted by `R/query_parse.R`. Query builders must
>   emit the same AST that the parser emits.
> - Do NOT port deprecated wrapper functions (`And`, `AndNot`, `Or`,
>   `OrNot`, `Xor`, `XorNot`, `Lookup`, `Fetch`, `MaskSlice`,
>   `SquareMaskColumn`, `SquareMaskRow`).
> - Do NOT attempt `h5df` backend; defer.
> - Do NOT port `set_seed`, `setup_daf`, `install_daf_packages`,
>   `load_daf_packages`, `use_default_julia_environment` —
>   Julia-only glue.
> - Do NOT rename native's `contract_scalar`/`contract_vector`/
>   `contract_matrix`; keep native semantics, the wrapper's
>   same-named functions have different signatures.
> - Do NOT drop the slice-ledger NEWS entries; fold them under a
>   `## Development history` header.
> - `R CMD INSTALL . --preclean` before any bake-off; bake-off
>   `empty_cache` calls are load-bearing.
