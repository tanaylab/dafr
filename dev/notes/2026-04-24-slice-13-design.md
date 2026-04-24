# Slice 13 — Design: Full h5ad Feature Coverage

**Date:** 2026-04-24
**Predecessor:** Slice 12 (tag `slice-12-filter-matrix` on `main`).
**Scope:** Extend h5ad I/O (slice-10b) to handle the three most-used feature categories currently skipped: sparse matrix encoding, categorical obs/var columns, and nested `uns` groups. Items `obsm/varm/obsp/varp/raw` remain deferred (dimensional mismatch with Daf's axis model).

## 1. Goal

Drop the three most-impactful "skip+warn" paths in `h5ad_as_daf`/`daf_as_h5ad` and implement proper round-tripping.

**Done signal.** `h5ad_as_daf` and `daf_as_h5ad` round-trip a fixture containing: sparse CSR X, a categorical obs column, a nested uns dict — all without handler-warnings. Existing minimal-fixture round-trip from slice-10b stays green.

## 2. Out of scope

- `obsm` / `varm` / `obsp` / `varp` (axis-synthetic matrices). Handler-warned, skipped.
- `raw` group (pre-filter counts). Handler-warned, skipped.
- `anndata` version attribute bump handling beyond 0.1.0.

## 3. Locked decisions

| # | Topic | Decision |
|---|---|---|
| 1 | Sparse read | h5ad's `/X` or `/layers/*` may have attribute `encoding-type = "csr_matrix"` or `"csc_matrix"`. Read the three datasets `data`, `indices`, `indptr` under the group. Construct a `Matrix::dgCMatrix` — transposing to CSC if source was CSR (since h5ad CSR uses obs × var, which matches our intended (obs, var) orientation, but `Matrix` CSC packs by column). For CSR source with shape (n_obs, n_vars): build a `dgRMatrix` equivalent then convert via `as(mat, "CsparseMatrix")`. |
| 2 | Sparse write | If `get_matrix` returns a `sparseMatrix`, write as CSC (`encoding-type = "csc_matrix"`). Three datasets: `data`, `indices`, `indptr`. Root of the group is the matrix; the attributes `encoding-type` and `encoding-version` live on the group object, `shape` attribute on the group as a 2-element int. Keep dense write path for dense matrices. |
| 3 | Categorical read | A column group with sub-datasets `codes` (int) and `categories` (character) + attribute `ordered` (bool). Read both, construct an R factor (if ordered) or character vector. For Daf, store as a character vector (factor levels lost — stored entries preserved). Raise via the `unsupported_handler` if `ordered = TRUE` is important for downstream fidelity. |
| 4 | Categorical write | If a Daf vector is a factor, write as the categorical group with `codes` (0-based int) + `categories` (levels). Always write with `ordered = FALSE` (R character vectors don't carry ordering info). Daf character columns with low cardinality MAY be offered as categorical via a heuristic — for simplicity, only factor inputs produce categorical output. |
| 5 | Nested uns | On read, recurse into nested groups within `/uns`. Flatten keys via `_` separator so `/uns/moments/n_counts` becomes Daf scalar `moments_n_counts`. On write, heuristic: if a Daf scalar's name contains `_` AND another scalar shares a common prefix, nest. Simpler: no nested write in this slice — Daf flat scalars map to flat `/uns/*`. Document asymmetry in NEWS. |
| 6 | Phase ordering | A (sparse) → B (categorical) → C (nested uns read) → Z (polish). |
| 7 | Test fixture extension | Extend the generator script to ALSO produce `sparse_test.h5ad` (sparse X, categorical obs column, nested uns). Commit both. |

## 4. Test plan

~40 new assertions across `test-anndata-format.R` (sparse + categorical round-trip) and `test-anndata-nested-uns.R` (new file).

Must remain green: the existing `test-anndata-format.R` minimal-fixture round-trip.

## 5. Execution order

- Phase 0: branch `slice-13-h5ad-full` off main.
- Phase A: sparse read/write + tests + extended fixture.
- Phase B: categorical read/write + tests.
- Phase C: nested uns read + tests.
- Phase Z: NEWS entry; merge; tag `slice-13-h5ad-full`; exit note.

## 6. Exit criterion

- ~2684 PASS (2644 + ~40).
- `devtools::check()` 0E/0W/<5N (no regression).
- Fixture `sparse_test.h5ad` committed to `inst/extdata/`.
- Merged + tagged.
