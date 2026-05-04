# Slice — Julia parity beyond queries.jl, names-on-every-return — Kickoff breadcrumb

**Date:** 2026-05-04
**Branch hint:** `slice-julia-parity-named-returns` (or numbered if the
sequence is being kept consistent — slice-19/20 are unused).
**Predecessor:** v0.3.0 release on `main` (literal queries.jl port + B4-B6
+ E1, E2). Outstanding parity gaps catalogued in
`dev/notes/2026-05-03-queries-jl-parity-divergences.md` (E3-E11, B7-B9,
API1, N1) — N1 (no dimnames on returns) is *the* through-line of this
slice.

## Motivation

Two intertwined gaps justify a single slice:

1. **Names contract is not enforced.** The user-facing memory note
   (`feedback_format_api_named.md`) says:
   > format_get_vector / format_get_matrix and anything that has names
   > must return them; do not strip names in lower layers.

   Today the format-API layer **strips dimnames** in `format_set_*` (so
   storage is canonical: axis names live on the axis, not on the value)
   and **does not reattach** in `format_get_*`:

   - `R/memory_daf.R:370-372` — `format_get_matrix` returns dimnames =
     `list(NULL, NULL)`.
   - `R/files_daf_read.R:545,564`, `R/zarr_format.R:839-941` — same
     pattern.
   - `R/utils.R::.validate_vector_value:79` — strips `names(vec)` on
     `format_set_vector`.

   `query_eval.R` papers over this by re-attaching `out_rownames` /
   `out_colnames` after the fact (closing N1 from the *query* side),
   but every other consumer of `format_get_*` — `copies.R`,
   `concat.R`, `chain_daf.R`, `view_daf.R`, the public `get_vector` /
   `get_matrix` / `get_axis`, `as_anndata`, `get_dataframe` — silently
   returns unnamed values. The Julia API never strips: `NamedVector`
   / `NamedMatrix` carry dimnames at every layer.

2. **Beyond queries.jl, parity coverage is uneven.** dafr has well-
   ported queries.jl (`tests/testthat/test-query-*.R`), readers/writers
   (`test-readers-julia-examples.R`, `test-writers-julia-examples.R`),
   and a few topical Julia compats (chain, copies, view, files,
   adapter, dataframes). The remaining DAF.jl test files have *no*
   port:

   | Julia test       | Lines | dafr port |
   |------------------|------:|-----------|
   | `data.jl`        | 4329  | partial — readers/writers only |
   | `contracts.jl`   | 1639  | partial — `test-contracts-*.R` exist |
   | `copies.jl`      | 1397  | partial — `test-copies-*.R` exist |
   | `read_only.jl`   |  764  | none |
   | `views.jl`       |  654  | partial — `test-view-julia-compat.R` |
   | `operations.jl`  |  576  | partial — registry tests only |
   | `chains.jl`      |  537  | partial — `test-chain-julia-compat.R` |
   | `reorder.jl`     |  500  | partial |
   | `concat.jl`      |  474  | partial |
   | `computations.jl`|  434  | none |
   | `mmap_zip_stores.jl` | 246 | partial |
   | `cache_groups.jl`|  180  | partial |
   | `anndata.jl`     |  157  | partial |
   | `tokens.jl`      |  146  | partial — `test-query-tokens.R` |
   | `reconstruction.jl` | 114 | none |

   Most of these test files exercise `Vector{T}` / `NamedVector` /
   `NamedMatrix` returns whose names ARE asserted in Julia. A literal-
   ish port surfaces every place where dafr's contract drift becomes
   visible — i.e. it's the right driver for closing N1 broadly.

## Scope (3 sub-slices)

### S1 — Names everywhere (close N1)

Lift the format-API contract: **every `format_get_*` and every public
return must carry names**. Specifically:

- `format_get_vector(daf, axis, name)`: returns a vector with
  `names = format_axis_array(daf, axis)$value`. Implement once at
  the format_api dispatch level (helper or wrapper) so memory /
  files / zarr / http / chain / view all benefit; deletion of the
  `Dimnames = list(NULL, NULL)` lines in the leaf adapters then
  becomes a cleanup.
- `format_get_matrix(daf, rows, cols, name)`: returns with
  `dimnames = list(rows_entries, cols_entries)`. Same lift.
- Public `get_vector(daf, axis, name)` / `get_matrix` /
  `get_axis(daf, axis)` mirror the format-API contract on the
  named return. Audit `copies.R`, `concat.R`, `view_daf.R`,
  `chain_daf.R` for `format_get_vector(...)$value` patterns that
  currently drop the surrounding name attribute and either preserve
  the names through the operation or restate them at the public
  boundary.
- `query_eval.R::.apply_lookup_vector` / `.apply_lookup_matrix`:
  the manual `out_rownames` / `out_colnames` reattachment becomes
  redundant — names already arrive named. Delete the workaround.

Tests: extend `test-format-api-contract.R` (new file or fold into
existing `test-format-api.R` if present) — for every backend
(memory, files, zarr, http) assert:
- `format_get_vector` returns a named vector with names matching the
  axis entries (in axis order).
- `format_get_matrix` returns a matrix / dgCMatrix with rownames =
  rows-axis entries, colnames = cols-axis entries.

Plus an integration regression test: `get_vector`, `get_matrix`,
`get_axis`, `get_query`, `get_dataframe` on a memory_daf, then
round-tripped through `files_daf` / `zarr_daf` / `as_anndata` —
names survive every hop.

Risk: surface area. Every adapter changes. Existing tests that
asserted unnamed returns (e.g. some `expect_equal(..., c(1, 2, 3))`
without names) need updating to the named form. The compensating
change in query_eval is ~30 lines deleted, but the ripple is broad.

### S2 — Port the remaining Julia test files, focused on the named-
return assertions

Don't try to ship a literal port of the 4329-line `data.jl` in one
shot. Instead, walk each Julia test file, *extract* the named-return
assertions (search for `Pair{`, `NamedVector`, `NamedMatrix`,
`names(`, `dimnames(`), and port those. Sequence the files by
size + smallest-blast-radius:

- **read_only.jl (764 lines)** — no current port; covers every
  read-only wrapper (`ReadOnlyChainDaf`, `ContractDaf`-as-reader,
  `ViewDaf`-as-reader). Their `format_get_*` go through the wrapper
  layers; an unnamed leak in any one of them surfaces here.
- **computations.jl (434 lines)** — no port. Computation contracts
  produce intermediate `NamedVector` / `NamedMatrix` results; this
  is where contract-verify code paths live.
- **concat.jl (474 lines)** — `concat_axis` is documented to produce
  a `NamedVector` with the concatenation order's entries. dafr's
  `concat.R:211,260,353,375` unwraps `$value` and discards names.
- **reorder.jl (500 lines)** — `reorder_axes` returns the daf
  itself; subsequent `format_get_vector` should still return names
  in the new axis order.
- **chains.jl (537 lines)** — partial port exists; extend with the
  named-return assertions.
- **views.jl (654 lines)** — partial port exists; extend.
- **contracts.jl (1639 lines)** — partial port exists; named-return
  asserts via `verify_input_contract` / `verify_output_contract`.
- **operations.jl (576 lines)** — registry + named-output checks
  on reductions / eltwise; mostly already covered by R-side tests
  but needs grep against `Pair{` for missing assertions.
- **data.jl (4329 lines)** — biggest; most overlaps with R-side
  format-API tests; defer to S3 unless a smaller subset emerges.

For each ported test, the port file lives at
`tests/testthat/test-<julia-name>-jl-parity.R` (mirrors the
queries.jl-parity convention). Document any divergence in a sister
file `dev/notes/2026-05-04-<julia-name>-jl-parity-divergences.md`.

### S3 — `data.jl` literal port (defer if S1+S2 already buy enough
parity; pick up in a sibling slice)

`data.jl` is the storage-API stress-test (axes, vectors, matrices,
relayout, sparse, type coercion, error messages). Many of its
assertions probably already pass; the long tail is where the bugs
hide. Treat it like queries.jl — a port-and-catalogue pass with
small fix-as-you-go behaviour items, plus a deferred catalogue for
structural divergences. Run after S1 (so the names contract is
already enforced) and S2 (so the wrapper layers are already
covered).

## Sequencing notes

- **S1 first.** The names contract is the through-line; doing it
  first means the S2 ports just write what Julia tests already
  write (named asserts) without first having to relax them to
  `unname()`. Doing S1 last is feasible but doubles the test churn
  (every test gets rewritten twice).
- **S1 needs a single-PR mindset.** It's a coordinated change
  across `R/format_api.R` + every adapter + every consumer; a
  half-done S1 is noisier than no-S1. Keep it as one slice with no
  intermediate ship to main.
- **S2 is parallelisable across files.** Each Julia test file ports
  independently. If the workload feels heavy, hand off the smallest
  files (anndata, cache_groups, mmap_zip_stores, tokens) to a
  subagent.
- Keep B4-B6 / E1 / E2 from v0.3.0 as the floor — don't regress.

## Validation

- After S1: every `format_get_*`-direct test asserts names; every
  `get_query` integration test still passes; the workaround in
  `query_eval.R::.apply_lookup_vector` / `.apply_lookup_matrix` is
  removed (or downgraded to a no-op `stopifnot(!is.null(names(...)))`).
- After S2: `tests/testthat/test-*-jl-parity.R` files exist for each
  ported file. Skip count attributable to the new ports starts
  bounded (we know what we're skipping); shrinks as evaluator gaps
  close.
- A round-trip integration: `memory_daf` →
  `files_daf` (write+reopen) → `zarr_daf` (write+reopen) →
  `as_anndata` (write+reopen) → `as_h5ad` (write+reopen). After each
  hop, every `get_vector` / `get_matrix` / `get_axis` /
  `get_query` / `get_dataframe` returns the expected names.

## Out of scope

- Cosmetic error-text alignment with Julia (carat-aligned context,
  exact wording). T1-T5 in the queries divergences doc are still
  text-only divergences.
- E3-E11 from queries (matrix-slice-as-mask, top-level comparator,
  etc.). They're real but orthogonal to names.
- Windows MmapZipStore stub failure (separate platform-build slice).
- Performance micro-tuning for the names attribute (R's named-vector
  attribute is cheap — vector of strings; matrix dimnames are two
  vectors. Don't over-engineer).

## Risk register

- **Existing R-side tests assert unnamed returns.** Some
  pre-2026-04 tests (`test-query-eval-lookups.R` precedent excepted)
  may compare against `c(1, 2, 3)` without names. Inventory pass
  before flipping the contract; rewrite those assertions to the
  named form. Estimated 20-50 sites.
- **Sparse-matrix dimnames.** `Matrix::dgCMatrix` carries `@Dimnames`
  separately from a dense matrix's `dimnames(...)`. The leaf adapters
  set `@Dimnames = list(NULL, NULL)` explicitly — equivalent
  unsetting on dense is `dimnames(...) <- NULL`. Both must be
  reverted in S1.
- **bit64 integer64 vectors.** `bit64::as.integer64` strips names on
  some operations (the existing `.op_convert` in `R/operations.R`
  already handles this for matrices via explicit dim/dimnames
  restore). Worth a targeted test in S1.
- **AnnData round-trip.** `as_anndata` / `as_h5ad` layer adds
  another names-stripping risk via the categorical / obs / var paths.
  Keep an explicit names-survive test through the h5ad write/read.
