# Slice 1 exit gate — 2026-04-20

## Deliverables

- [x] `MemoryDaf` S7 class + `memory_daf(name)` constructor (Phase A)
- [x] `format_*` S7 methods on MemoryDaf — 22 methods across 5 sections (axes query/mutation, scalars query/mutation, vectors query/mutation, matrices query/mutation, matrix relayout) (Phases B1–F3)
- [x] User-facing read API — `has_axis`, `axes_set`, `axis_length`, `axis_vector`, `axis_entries`, `axis_indices`, `axis_dict`, `has_scalar`, `scalars_set`, `get_scalar`, `has_vector`, `vectors_set`, `get_vector`, `has_matrix`, `matrices_set`, `get_matrix`, `description` (Phases B3, C3, E1/E2, G1, H1)
- [x] User-facing write API — `add_axis`, `delete_axis`, `set_scalar`, `delete_scalar`, `set_vector`, `delete_vector`, `set_matrix`, `delete_matrix`, `relayout_matrix` (Phases B3, C3, E2, G2)
- [x] Cache infrastructure — version stamps (`axis_stamp`, `vector_stamp`, `matrix_stamp`), stamp-based `cache_lookup` / `cache_store`, LRU eviction under memory cap (mapped tier exempt), `empty_cache` with Julia-style `clear=`/`keep=` accepting both short (`"memory"`) and capitalised (`"MemoryData"`) tier names (Phase I)
- [x] `bit64::integer64` round-trip tests (named reorder preserves class) + `cli::cli_inform` optional verbose messages via `dafr.verbose = TRUE` (Phase J) — closes unused-Imports NOTEs for both packages
- [x] `is_altrep()` relocated from package namespace to `tests/testthat/helper-altrep.R` (Task K1)
- [x] `const_cast<void*>` SAFETY comments added to three sites in `src/altrep_mmap.cpp` (Task K2)
- [x] OpenMP-branch regression tests for `kernel_log_add_cpp` + `kernel_csc_colsums_cpp` (Task K3)
- [x] Test port from DafJuliaWrapper — scalars + axes coverage-gap fills (L1), vector + matrix coverage-gap fills (L2), cache behaviour (L3), writer cases (L4 — reference was entirely Julia-specific, no port needed; secondary coverage already present)

## Test + build status

- `testthat::test_dir("tests/testthat")` — **470 PASS / 0 FAIL / 0 SKIP / 1 WARN**. The single warning is the pre-existing `scran::quickCluster` / `irlba::irlba` SVD tolerance notice in `test-altrep-downstream.R`, which predates Slice 1 and is not a dafr defect.
- `devtools::check(error_on = "note", manual = FALSE, vignettes = FALSE)` with `_R_CHECK_SYSTEM_CLOCK_=0` — **0 ERROR / 0 WARNING / 0 NOTE**. Without the env var the only residual is the environmental "unable to verify current time" NOTE caused by `worldclockapi.com` returning HTTP 503; this is not a package issue and CI will hit its own network.
- `pkgbuild::compile_dll(debug = FALSE)` — clean on linux-x86_64. C++ sources unchanged in this slice apart from the K2 comment-only edit in `src/altrep_mmap.cpp`.

## Bake-off / perf items

No new benchmarks this slice — Slice 1 delivers the API surface, not the hot-path kernels. The Slice 0 bake-off decision (cpp11 + BLAS, with transpose re-evaluation deferred) remains in force.

## Scope closed vs deferred

**Closed in Slice 1** (from the Slice 0 kickoff "Still open" list):

- `bit64` / `cli` unused-Imports NOTEs — closed via `@importFrom` in `R/dafr-package.R` and real call sites in `R/cache.R` / `R/memory_daf.R`.
- `is_altrep()` exposure via `dafr:::` — relocated to test helper.
- Pre-existing Rd undocumented-args WARNING on S7 classes — closed alongside user-facing API documentation.
- `const_cast<void*>` safety comments on mmap read-only path — added.
- OpenMP kernel branch has direct test coverage (previously zero).

**Deferred to Slice 2+**:

- G2 upstream PR for FilesDaf on-disk spec — pending user consent.
- CSC colSums bake-off re-run at 100M+ nnz — needs dataset larger than SMALL=1.
- Transpose kernel B-vs-D decision — real-world transpose usage still not materialised.
- Julia FilesDaf findings (no on-disk version counters, no atomicity) — design question for Slice 2 when FilesDaf lands.
- `writeBin(..., size = 8L)` endianness pin — FilesDaf concern.
- Long-vector (>2^31) and "file truncated while R vector live" ALTREP scenarios — later slices.
- `dafr.omp_threshold` option is declared but not yet consumed by the C++ kernels (which use hardcoded thresholds of 10000 for log_add and 1000 for csc_colsums) — wire up when Slice 3's eltwise/reduction kernels land.
- `get_vector(..., default = <axis-length vector>)` currently recycles per `rep()` semantics instead of pass-through; mirror Julia's "accept vector-length default as-is" when that discrepancy bites a consumer.

## Commit history

Slice 1 landed as 34 commits on branch `slice-1-memory-daf` (off `main` at tag `slice-0`). Each task ran through spec-compliance and code-quality reviewers; several tasks got follow-up commits for reviewer-flagged polish (`is.character` guard restore for B2, DRY `.assert_name`/`.assert_flag` helpers for B3/E2, axis-counter invalidation test for E1, primary/flipped cache-share pin for G1).

## Decision to enter Slice 2

**Go.** MemoryDaf binds the entire S7 API to real storage, the cache carries version-stamped invalidation with LRU+memory-cap enforcement, and the user-facing surface is exercised end-to-end via the ported test suite. Slice 2 (FilesDaf + mmap + Julia bidirectional compat + readBin fallback) has no blocking dependencies on Slice 1 internals — the `format_*` contract is the stable interface.

## Next plan

Slice 2 — FilesDaf backend with mmap + readBin fallback + bidirectional Julia compatibility. Re-open the Slice 0 FilesDaf on-disk spec draft at `dev/specs/filesdaf-on-disk-spec-draft.md` and settle the three `[UNCLEAR]` markers before writing code.
