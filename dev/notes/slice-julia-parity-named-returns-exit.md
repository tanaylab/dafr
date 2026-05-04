# Slice — Julia parity: names everywhere on `format_get_*` — Exit note

**Date:** 2026-05-04
**Branch:** `slice-julia-parity-named-returns` → fast-forwarded into `dev` (tip `5ecf224`); pushed to `private/dev`. Local slice branch and worktree removed.
**Kickoff / plan:** `dev/notes/2026-05-04-slice-julia-parity-named-returns-kickoff.md`, `dev/notes/2026-05-04-slice-julia-parity-s1-names-everywhere-plan.md`.

## Scope delivered (S1 only)

The S1 sub-slice from the kickoff: enforce the "named returns" contract at the format-API layer for every backend on `dev`. S2 (port remaining Julia test files) and S3 (literal `data.jl` port) deferred to follow-up slices.

- **New format-API contract.** `format_get_vector(daf, axis, name)` returns a named atomic vector with `names = format_axis_array(daf, axis)`. `format_get_matrix(daf, rows_axis, columns_axis, name)` returns a dense matrix or `dgCMatrix`/`lgCMatrix` whose dimnames are `list(rows-axis entries, cols-axis entries)`.
- **Two helpers** in `R/utils.R`: `.attach_vector_axis_names(daf, axis, vec)` and `.attach_matrix_axis_dimnames(daf, rows_axis, cols_axis, mat)`. Length-strict; matrix helper branches on `dgCMatrix`/`lgCMatrix` for `@Dimnames` slot vs base `dimnames<-`.
- **Backends wired:** `MemoryDaf` (`R/memory_daf.R`), `FilesDaf` / `FilesDafReadOnly` (via the cached helpers in `R/files_daf_read.R`). Wrappers `ReadOnlyChainDaf` / `WriteChainDaf` / `ContractDaf` / `ViewDaf` inherit by delegation.
- **C-level ALTREP fix.** `Duplicate_method` registered for each mmap ALTREP class (`MmapRealClass`, `MmapIntClass`, `MmapLglClass`) in `src/altrep_mmap.cpp` so R's `names<-` (which duplicates first, then setAttribs) shares the immutable mmap region instead of materializing. `is_altrep(v)` survives `names(v) <- entries`.
- **Consumer cleanup.**
  - `R/readers.R::get_vector` no longer reattaches names defensively; `get_matrix` no longer post-assigns dimnames after the flipped-layout transpose (`Matrix::t()` and `t()` swap dimnames automatically).
  - `R/query_eval.R::.apply_chained_lookup_vector`'s `pivot_values`-naming workaround is replaced with an internal-contract assertion.
- **Bug fixes surfaced by S1.**
  - `R/concat.R::.concat_axis_vector` (line 246) and `R/concat.R::.concat_merge_vector` (line 353) now `unname()` at the `format_set_vector(...)` boundary; intermediate names from the named getter were tripping `.validate_vector_value`'s axis-membership check on prefix-rewritten destination axes.
  - `R/copies.R::.copy_vector` (line 233) — same `unname()` defensively at the shared setter call (preventive; no test reproducer surfaced, audit found no concrete failure path on current branches).
- **Test suite:** new contract file `tests/testthat/test-format-api-named-returns.R` (memory + files + chain + contract + view + round-trip + as_anndata round-trip — 35 PASS). Existing assertion updates in `test-{memory,files}-{vectors,matrices}.R`, `test-query-eval-lookups.R`, `test-view-daf.R`, `test-altrep-mmap.R`, `test-concat.R`, `test-copies-vector.R`.

## Numbers

- **Full suite (`NOT_CRAN=true Rscript testthat.R`):** `FAIL 0 | WARN 1 | SKIP 71 | PASS 3237`. Net +37 PASS over the pre-S1 baseline (the contract tests plus the ALTREP regression tests plus the post-Task-8 assertion updates).
- The single WARN is pre-existing BiocSingular/irlba noise; not introduced by this slice.

## Why no main ship

Main and dev have diverged structurally beyond what the slice's commits assume:
- Main's `format_get_*` returns `list(value, cache_group)` (the `MEMORY_DATA` / `MAPPED_DATA` / `QUERY_DATA` enum mirroring `DataAxesFormats.jl::Formats::CacheGroup`).
- Dev's `format_get_*` returns the bare value.

The slice's helpers and consumer-side cleanups are written against dev's bare-value contract. Cherry-picking them onto main would call `names(list(...)) <- entries` — wrong target. A faithful landing on main requires re-doing the slice against main's `cache_group_value` shape (and extending to main-only backends like `R/zarr_format.R`, `R/http_format.R`, `R/reorder.R`) — that's a separate slice, not a cherry-pick. Recorded for future S1-on-main work.

## Per-phase commit SHAs (on `dev`, fast-forwarded from `slice-julia-parity-named-returns`)

- `14be6e9` test(parity): failing contract tests for named format_get_* (memory)
- `9b598aa` feat(format-api): helpers to attach axis-entry names on get returns
- `1fd2494` feat(format-api): MemoryDaf format_get_* return named values
- `0b7a9bd` feat(format-api): FilesDaf format_get_* return named values
- `ab2fbc2` feat(altrep): Duplicate_method preserves mmap ALTREP across attr sets
- `964e4f7` test(parity): wrapper backends inherit named format_get_* contract
- `b2e96a5` refactor(readers): drop redundant name reattachment now that format_get_* returns named
- `5559eed` refactor(query): assume named pivot vectors from named format_get_vector
- `e466b13` test: align memory-vector assertions with named format_get_* contract
- `822ba62` test: align memory-matrix assertions with named format_get_* contract
- `30eac2f` test: align files-* assertions with named format_get_* contract
- `a1a4708` test: align query-eval and view-daf assertions with named contract
- `52cb04d` fix(concat): drop intermediate names before format_set_vector
- `2996b1b` fix(copies): drop intermediate names before format_set_vector
- `62b69a7` test(parity): names survive memory -> files -> get_query (and anndata when available)
- `b6b3127` news: S1 — names everywhere on format_get_* (unreleased)
- `5ecf224` release: revert version bump — dafr stays at 0.2.0

## Follow-ups

- **S2** (kickoff scope): port remaining Julia test files (`read_only.jl`, `computations.jl`, `concat.jl`, `reorder.jl`, `chains.jl`, `views.jl`, `contracts.jl`, `operations.jl`) for named-return assertions. Each file is independently shippable; can parallelise via subagents.
- **S3** (kickoff scope): literal `data.jl` (4329 lines) port — biggest, most parser-and-storage stress, deferred behind S2.
- **S1-on-main:** re-do the names-everywhere contract against main's `cache_group_value` shape, extending to zarr / http / reorder backends. New plan needed; the dev plan is the structural template but does not transcribe.
