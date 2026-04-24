# Slice 10c — Design: Small Ports (Wrapper-Parity Surface)

**Date:** 2026-04-23
**Predecessor:** Slice 9d-N (tag `slice-9d-n` on `main`).
**Parent kickoff:** `dev/notes/slice-10-kickoff.md` §"10c — small ports".
**Analysis doc:** `dev/notes/2026-04-22-slice-10-wrapper-parity-analysis.md`.
**Scope:** Ship 25 user-facing exported functions across 7 groups
(+1 export removed: `get_frame` renamed to `get_dataframe_query`) that
close the surface-level gap between native `dafr` and the Julia-facade
wrapper at `~/src/dafr`. Pure R; no C++ touches; no new hard deps.

## 1. Goal

Export the wrapper-parity surface that users expect to find when they
migrate from `dafJuliaWrapper`: handler constants, version counters,
group helpers, DataFrame helpers, query introspection, class-surface
predicates, and contract-UX constructors. Keep native's underlying
semantics (record-constructor contracts, AST-based queries, per-process
integer counters) — surface the wrapper's names and call shapes over
them. Document every deliberate divergence in NEWS.

**Done signal.** 25 new exports (4 handlers, 3 query-introspection, 3
version-counter, 3 group, 4 class-sugar, 3 dataframe, 5 contract-UX),
net NAMESPACE delta +24 after `get_frame` removal; ~130 new testthat
assertions; full
existing suite green under `cd tests && NOT_CRAN=true Rscript testthat.R`;
`R CMD INSTALL .` clean; `devtools::check()` NOTE-free on the new
exports; merged to `main` with tag `slice-10c`.

## 2. Out of scope

- Slice-10a query builders (`Axis()`, `LookupVector()`, ...). Separate slice.
- Slice-10b AnnData facade + h5ad round-trip. Separate slice.
- Slice-10d docs + vignettes + release polish. Separate slice.
- `h5df` HDF5-backed Daf store. Deferred to post-release.
- `set_seed` / Julia lifecycle glue. Inapplicable to native.
- Any C++ / kernel changes. This slice is R-only.
- `@examples` blocks for PRE-EXISTING exports (the ~25 carry-over from
  9d-M). 10c adds `@examples` on NEW exports only; 10d sweeps the rest.
- Full re-audit of `R/contracts.R` record-constructor semantics. The
  ~30-line `tensors` schema extension is the only slice-7 touch.

## 3. Locked decisions

| # | Topic | Decision |
|---|---|---|
| 1 | `read_only(daf, name = NULL)` | 1-element chain: `chain_reader(list(daf), name = name %||% daf_name(daf))`. No new S7 class. |
| 2 | `create_contract` signature | `create_contract(scalars = list(), vectors = list(), matrices = list(), tensors = list(), axes = list(), is_relaxed = FALSE)`. Typed per-category; no `name` field (computation label lives on `ContractDaf` via `contractor()`). Diverges from wrapper's flat-`data`-list shape. |
| 3 | `tensor_contract` signature | `tensor_contract(main_axis, rows_axis, cols_axis, name, expectation, type, description)`. Three explicit axes; param `type` (not `dtype`) to match native's existing `contract_scalar/vector/matrix` family. |
| 4 | `get_frame` collision | Rename existing native `get_frame` → `get_dataframe_query`. Remove `get_frame` from NAMESPACE. Add `get_dataframe(daf, axis, columns = NULL, cache = TRUE)` (axis-NAME form) as a new export. Pre-1.0 — no deprecation shim. |
| 5 | Version counter return type | Return `integer(1)`. Wrapper returns stringified `UInt32`; native integer is fine within a session and types-correct. NEWS entry notes the wrapper deviation. |
| 6 | File layout | Hybrid. 3 new files: `R/groups.R`, `R/contract_ux.R`, `R/dataframes.R`. Extend 6 existing: `R/handlers.R`, `R/cache.R`, `R/classes.R`, `R/complete.R`, `R/query_ast.R`, `R/queries.R`, `R/chain_daf.R`, `R/contracts.R`. |
| 7 | Slice ordering | Safest-first: handlers → query introspection → version counters → group helpers → class sugar → dataframes → contract UX. Contract UX last because it is the highest-risk group (largest surface, couples to record-constructors + ContractDaf methods + S7 schema extension). |
| 8 | Tensor contract support | Zero S7 schema change. `Contract` already has a flat `data` slot with `$kind`-dispatched records; `tensor_contract` returns a `$kind = "tensor"` record; `create_contract` concatenates typed args into `data`. Extend `.data_key`, `.verify_contract`, `.verify_access`, `.merge_types`, and the validator to handle `kind = "tensor"` (~50 lines total in `R/contracts.R`). Tensor records carry `$main_axis`, `$rows_axis`, `$columns_axis`, `$name`, `$expectation`, `$type`, `$description`. |
| 9 | Group hash | Hand-rolled FNV-32 in R (`~15 lines` in `R/groups.R`). No `digest` / `rlang::hash` dep. Stable across sessions; deterministic for same input. |
| 10 | Dependency adds | `rlang` → `Imports` (for `check_installed` in `get_tidy`). `tidyr`, `tibble` → `Suggests` (gated). No other DESCRIPTION changes. |
| 11 | `get_tidy` missing dep | Hard-error via `rlang::check_installed(c("tidyr", "tibble"))`. Never silent fall-through. |
| 12 | `escape_value`/`unescape_value` | Round-trip identity is non-negotiable. `unescape_value(escape_value(s)) == s` for every legal query literal. Tested across ≥30 cases including `.FORBIDDEN_NAME_CHARS` and Unicode. |
| 13 | `is_daf` semantics | Predicate: `S7::S7_inherits(x, DafReader)` returning `TRUE`/`FALSE`. Never errors on non-daf input. |
| 14 | `daf_name(x)` | Asserts `is_daf(x)` first, then `S7::prop(x, "name")`. Erroring predicate sibling of `is_daf`. |
| 15 | `complete_path` | Thin public alias of existing internal `.complete_path`. |
| 16 | Test file layout | One testthat file per export group (7 files total). Follows repo convention. |

## 4. Surface contract (per export group)

### 4.1 Handler constants + shim (`R/handlers.R`, +~30 lines)

```r
#' @export
ERROR_HANDLER <- "error"
#' @export
WARN_HANDLER <- "warn"
#' @export
IGNORE_HANDLER <- "ignore"

#' @export
inefficient_action_handler <- function(handler) {
    register_dafr_handler("inefficient", handler)
    invisible()
}
```

Constants are the literal strings already accepted by
`register_dafr_handler`. No second registry. `inefficient_action_handler`
accepts any value `register_dafr_handler` accepts (string or function).

### 4.2 Query introspection (extends `R/query_ast.R` + `R/queries.R`, +~60 lines)

- `escape_value(s)` — public alias of `.escape_value`. Add `@export`.
- `unescape_value(s)` — inverse. Semantics: invariant
  `unescape_value(escape_value(s)) == s` for all legal literals.
- `query_requires_relayout(daf, query)` — AST walk returning `TRUE` if
  evaluation requires a matrix transpose. Lives in `R/queries.R` next
  to `query_result_dimensions`.

**Relayout detection rule.** A query requires relayout iff its AST
contains a `LookupMatrix` / `ReduceToRow` / `ReduceToColumn` node whose
effective rows-axis / columns-axis order (after any `@` axis scopes)
differs from the stored order in the daf. The AST-scan logic follows
the same rows/cols tracking used in `.collect_query_versions` (see
`R/queries.R:38+`).

### 4.3 Version counters (extends `R/cache.R`, +~60 lines)

Three accessors returning `integer(1)`:

```r
#' @export
axis_version_counter <- function(daf, axis) {
    S7::prop(daf, "axis_version_counter")[[axis]] %||% 0L
}

#' @export
vector_version_counter <- function(daf, axis, name) {
    S7::prop(daf, "vector_version_counter")[[paste0(axis, ":", name)]] %||% 0L
}

#' @export
matrix_version_counter <- function(daf, rows_axis, columns_axis, name) {
    S7::prop(daf, "matrix_version_counter")[[
        paste0(rows_axis, ":", columns_axis, ":", name)
    ]] %||% 0L
}
```

Wrap the existing internal counters directly; do not use
`axis_stamp`/`vector_stamp`/`matrix_stamp` (those compose multiple
counters for cache-key stamps). Missing objects return `0L` — not an
error — matching the wrapper's "no such axis yet → 0" semantics.

### 4.4 Group helpers (`R/groups.R` new, ~120 lines)

- `compact_groups(group_indices)` → `list(n_groups = N, group_indices = ...)`.
  Renumber non-zero indices to 1..N in first-seen order; preserve 0.
- `collect_group_members(group_indices)` → list of integer vectors.
  Element `i` is the entries belonging to group `i`. Inverse of
  `compact_groups`'s output.
- `group_names(daf, axis, entries_of_groups, prefix)` → character
  vector of deterministic names. Same members → same name across
  sessions.

**`group_names` hash.** Hand-rolled FNV-32 on `sort(entry_names[group_members])`,
joined with `\x00`. Take 8 hex chars. Prefix-concat: `paste0(prefix, fnv_hex)`.
Pure R, no deps. FNV-32 constants: `offset = 2166136261`, `prime = 16777619`;
compute on UTF-8 bytes via `charToRaw`.

### 4.5 Class-surface sugar (extends `R/classes.R`, `R/complete.R`, `R/chain_daf.R`, +~60 lines total)

```r
# R/classes.R
#' @export
is_daf <- function(x) S7::S7_inherits(x, DafReader)

#' @export
daf_name <- function(x) {
    stopifnot("x must be a DafReader" = is_daf(x))
    S7::prop(x, "name")
}

# R/complete.R
#' @export
complete_path <- function(daf) .complete_path(daf)

# R/chain_daf.R
#' @export
read_only <- function(daf, name = NULL) {
    stopifnot("daf must be a DafReader" = is_daf(daf))
    chain_reader(list(daf), name = name %||% daf_name(daf))
}
```

### 4.6 DataFrame helpers (`R/dataframes.R` new, ~100 lines)

- `get_dataframe_query(daf, query, cache = TRUE)` — query-STRING form.
  Rename of existing `get_frame(daf, axis_query, columns = NULL)`,
  with `cache` flag added. Cache key: `query:<canonical(query)>`.
- `get_dataframe(daf, axis, columns = NULL, cache = TRUE)` — axis-NAME
  form. `axis` is an axis name (character scalar). Returns a data.frame
  with `rownames` = axis entries, one column per vector in `columns` (or
  all vectors if `columns = NULL`).
- `get_tidy(daf, axis, columns = NULL, cache = TRUE, ...)` — gated via
  `rlang::check_installed(c("tidyr", "tibble"))`. Calls
  `get_dataframe`, then `tidyr::pivot_longer(-name, names_to = "key", values_to = "value", ...)`,
  returns a `tibble`. `...` passes through to `pivot_longer`.

**Migration of `get_frame`.**
- Delete `get_frame` function + roxygen block from `R/queries.R`.
- Move body into a private `.get_dataframe_from_query(daf, query, cache)` helper in `R/dataframes.R`.
- `get_dataframe_query` calls the helper; `get_dataframe` builds a query
  like `sprintf("@ %s", axis)` (plus optional column selection) and delegates.
- Update any existing `test-queries.R` references to `get_frame` →
  `get_dataframe_query` (inline rename).

**Cache semantics.** `cache = TRUE` routes through the existing query
cache (stamp-invalidated). `cache = FALSE` bypasses — useful during
iterative work where the user has just mutated the daf.

### 4.7 Contract UX (`R/contract_ux.R` new, ~220 lines; +~70 lines in `R/contracts.R`)

```r
#' @export
create_contract <- function(scalars = list(),
                            vectors = list(),
                            matrices = list(),
                            tensors = list(),
                            axes = list(),
                            is_relaxed = FALSE) {
    # Validate each arg is a list of the right record type.
    # Construct a Contract() S7 object — existing constructor extended
    # with a `tensors` slot in this slice.
    Contract(
        scalars    = scalars,
        vectors    = vectors,
        matrices   = matrices,
        tensors    = tensors,
        axes       = axes,
        is_relaxed = is_relaxed
    )
}

#' @export
axis_contract <- function(name, expectation, description) {
    # Record-constructor; returns list with class "dafr_axis_contract"
    # for type-dispatch inside create_contract validation.
}

#' @export
tensor_contract <- function(main_axis, rows_axis, cols_axis, name,
                            expectation, type, description) {
    # Record-constructor; returns list with class "dafr_tensor_contract".
    # Stored in Contract@tensors; verified at verify_input/output time
    # by iterating main-axis entries (resolved against the daf at verify-time).
}

#' @export
contract_docs <- function(contract, format = c("markdown", "text")) {
    format <- match.arg(format)
    # Render contract to a single string: axes table, then four data tables.
    # Existing scalars/vectors/matrices get their existing render; tensors
    # gets a new render matching the same shape.
}

#' @export
verify_contract <- function(contract, daf) {
    # Single-pass: fresh ContractDaf + verify_input + verify_output.
    # Returns invisible(daf) on success; stops with diagnostic on failure.
    cd <- contractor("verify_contract", contract, daf, overwrite = FALSE)
    verify_input(cd)
    verify_output(cd)
    invisible(daf)
}
```

**Contract layering (revised).** Zero S7 schema change. `Contract` already
has a flat `data` slot with `$kind`-dispatched records (see
`R/contracts.R:57`). Approach:

- `create_contract(scalars, vectors, matrices, tensors, axes, is_relaxed)`
  concatenates `c(scalars, vectors, matrices, tensors)` into `data`
  and delegates to `Contract(name = "", is_relaxed = ..., axes = ..., data = ...)`.
  Typed args are validated per-list (each element's `$kind` must match
  its list).
- `tensor_contract` returns a record with `$kind = "tensor"`,
  `$main_axis`, `$rows_axis`, `$columns_axis`, `$name`, `$expectation`,
  `$type`, `$description`.
- Extend the `Contract` class `validator` to accept `kind = "tensor"` records.
- Extend `.data_key` to build a `"tensor:<main>:<rows>:<cols>:<name>"` key.
- Extend `.verify_contract` (at `R/contracts.R:780+`) to reconstruct
  tensor records from their key and dispatch to a new
  `.verify_tensor_data(cd, rec, is_for_output)` helper that iterates
  `format_axis_array(base, rec$main_axis)` and checks each per-entry
  matrix `"<entry>_<name>"` at `(rows_axis, columns_axis)`.
- Extend `.verify_access` to recognise the `"tensor"` kind.
- Extend `.merge_types` / merge_contracts data-key matching: no change
  needed (key-based dispatch already uses `.data_key`).

Existing scalars/vectors/matrices code paths untouched. Total surface
addition inside `R/contracts.R`: ~50 lines (helper + tensor verify walk
+ validator clause).

## 5. Error handling

| Group | Trigger | Response |
|---|---|---|
| Handlers | `inefficient_action_handler(42)` | `register_dafr_handler` existing guard fires (integer is neither string nor function). |
| Version counters | nonexistent axis / vector / matrix | returns `0L`, not an error. Matches wrapper "not-yet-tracked → 0". |
| Group helpers | non-integer `group_indices` | `cli::cli_abort("group_indices must be integer")`. |
| Group helpers | negative indices | `cli::cli_abort("group_indices must be ≥ 0 (0 denotes no group)")`. |
| Group helpers | `entries_of_groups` element out of axis range | `cli::cli_abort` with index. |
| DataFrame | `get_tidy` without tidyr/tibble | `rlang::check_installed(c("tidyr", "tibble"))` — install-hint error. |
| DataFrame | axis not on daf | existing `format_axis_array` guard. |
| DataFrame | `columns` element not a vector on axis | existing `format_get_vector` guard. |
| Class sugar | `is_daf(anything)` | never errors; returns `FALSE` on non-daf. |
| Class sugar | `daf_name(non_daf)` | `stopifnot` informative error. |
| Class sugar | `complete_path(non_daf)` | underlying `.complete_path` errors (existing behaviour). |
| Class sugar | `read_only(non_daf)` | `stopifnot` informative error. |
| Contract UX | wrong element class in `scalars`/`vectors`/`matrices`/`tensors`/`axes` | `cli::cli_abort` naming the offending position + expected class. |
| Contract UX | `verify_contract` failure | existing `verify_input`/`verify_output` error surfaces; no double-wrapping. |
| Contract UX | tensor main-axis missing on daf at verify-time | `cli::cli_abort("tensor '<name>' main_axis '<main>' not present on daf")`. |

## 6. Test plan

Budget: ~130 assertions across 7 files.

| File | Exports | Assertions | Coverage |
|---|---|---|---|
| `test-handlers-constants.R` | 4 | ~15 | Constants equal their lowercase aliases; `inefficient_action_handler("warn")` registers; round-trip via `emit_action`. |
| `test-query-introspection.R` | 3 | ~25 | `escape` @export works; `unescape∘escape == id` over ≥30 cases (spaces, quotes, backslashes, every `.FORBIDDEN_NAME_CHARS` member, Unicode); `query_requires_relayout` detects transpose, returns FALSE for same-order, errors on parse failure. |
| `test-version-counters.R` | 3 | ~15 | Initial value 0L; +1 after single mutation; unchanged after pure read; missing object → 0L; cross-object isolation (mutating axis X doesn't bump axis Y). |
| `test-groups.R` | 3 | ~15 | `compact_groups` round-trip identity on already-compact input; renumbers with gaps; preserves 0. `collect_group_members` inverse. `group_names` determinism: same members → same name; different members → different names; prefix applied. |
| `test-class-sugar.R` | 4 | ~15 | `is_daf` TRUE/FALSE on memory/files/chain/view/non-daf; `daf_name` extracts; `complete_path` returns existing; `read_only` rejects writes (via underlying chain_reader read-only semantics); `daf_name(read_only(d))` preserves name. |
| `test-dataframes.R` | 3 | ~15 | `get_dataframe` returns correct shape; rownames = axis entries; column subset respected; cache hit second call. `get_dataframe_query` equivalent to old `get_frame`. `get_tidy` errors when tidyr/tibble absent (via `withr::with_libpaths` + skipped-if-can't-simulate), produces tibble with columns `name`/`key`/`value` when present. |
| `test-contract-ux.R` | 5 | ~30 | `create_contract` returns Contract instance with all five typed slots; rejects wrong-type elements. `axis_contract`/`tensor_contract` record-shape + class. `contract_docs` returns character scalar, format="markdown" matches a regex. `verify_contract` green path + missing-axis red path + tensor main-axis present/absent. |

**Non-negotiable test mines.**

1. `unescape_value(escape_value(s)) == s` for every `s` in the 30+ case table.
2. Version counter: after one `set_vector` call, the vector counter increments by exactly 1.
3. Reading a vector does NOT increment its counter.
4. `group_names` same-members → same name across two calls on the same daf; and across two different dafs whose axis has the same entry names.
5. `read_only(d)` reads succeed; write calls error.
6. `daf_name(read_only(d, name = "foo")) == "foo"`.
7. `create_contract(tensors = list(tensor_contract(...)))` → `verify_contract(c, d)` succeeds iff main-axis is on `d`.
8. Existing slice-7 contract tests stay green (schema extension is backward-compatible — `tensors` slot defaults to empty list).

**Regression guard.** `test-queries.R` references to `get_frame` → `get_dataframe_query` inline-renamed as part of the slice.

## 7. Dependency changes

**`DESCRIPTION` edits.**

- Add to `Imports:` — `rlang`. Used for `rlang::check_installed` in `get_tidy`. Transitive today via `S7`/`cli`; making it explicit.
- Add to `Suggests:` — `tidyr`, `tibble`. Gated behind `check_installed` in `get_tidy`.

No `LinkingTo` / C++ / `SystemRequirements` changes.

**`NAMESPACE`** auto-regenerated by roxygen. Net change: +24 exports, −1 export (`get_frame`).

## 8. Slice execution order

Per locked decision #7:

1. **Handler constants + shim** (`R/handlers.R` extend, `test-handlers-constants.R` new). 10b unblocker.
2. **Query introspection** (`R/query_ast.R` + `R/queries.R` extend, `test-query-introspection.R` new). Round-trip identity locked early.
3. **Version counters** (`R/cache.R` extend, `test-version-counters.R` new). Mechanical.
4. **Group helpers** (`R/groups.R` new, `test-groups.R` new). Isolated pure-R port.
5. **Class-surface sugar** (`R/classes.R` + `R/complete.R` + `R/chain_daf.R` extend, `test-class-sugar.R` new). `read_only` lands here.
6. **DataFrame helpers** (`R/dataframes.R` new, `R/queries.R` rename, `test-dataframes.R` new, `test-queries.R` rename). Contains the `get_frame → get_dataframe_query` migration.
7. **Contract UX** (`R/contract_ux.R` new + `R/contracts.R` extend, `test-contract-ux.R` new). Largest group; lands under green suite.

Each step commits independently on a 10c feature branch. Suite must be
green after each step. Tag `slice-10c` on merge to `main`.

## 9. Exit criterion

- `R CMD INSTALL .` clean.
- `cd tests && NOT_CRAN=true Rscript testthat.R` green (per memory: test-helpers.R:26 is a known pre-existing skip/FAIL under Rscript; all other tests green).
- `devtools::check()` NOTE-free on the new exports (pre-existing NOTES are 10d-bucket and unblocked).
- Wrapper migration table in NEWS entry (breaking-change bullets for: get_frame rename, create_contract signature, tensor_contract param name `type`, version_counter integer return, create_contract no-`name`-field).
- Merged to `main`; tag `slice-10c`.

## 10. Known inflations / risks

- **Contract tensor support (dec #8)**: ~50 lines in `R/contracts.R`. Zero S7 schema change (flat `data` slot already exists); additions are to `.data_key`, `.verify_contract`, `.verify_access`, the `Contract` validator, and a new `.verify_tensor_data` helper.
- **`get_frame` rename**: one-way break. Pre-1.0 package; no deprecation shim. NEWS must document.
- **`get_tidy` cross-package gating**: `rlang::check_installed` is itself part of `rlang`. Add `rlang` to Imports directly, not rely on transitive.
- **`group_names` hash stability**: FNV-32 is byte-sensitive. Validate on UTF-8-normalised names; error (not silently diverge) on multi-byte characters with ambiguous normalisation. Cheap guard: assert `Encoding(x) %in% c("unknown","UTF-8")` before hashing.

## 11. Post-slice carry-over (unchanged from 9d-N)

- mmap S7-ctor floor (4 accept-class breaches).
- Two-pass flat-storage for mode/quantile per-cell overhead.
- Acc-struct slimming.
- `copy_all` double-write bug.
- 9d-M code-review minor items.

All deferred; independent of slice 10c.
