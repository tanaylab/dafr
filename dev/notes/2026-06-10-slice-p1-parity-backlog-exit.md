# Slice exit: P1 feature-blocked parity backlog (2026-06-10)

Worked the P1 "feature-blocked parity gaps" list from
`2026-06-10-post-0.4.2-kickoff.md`. Headline: **most of the backlog was
already closed in earlier slices**; the kickoff list was stale. Two items
were genuinely open and are now done (C1, M2); one is deferred (V1); the
rest were verified already-implemented and their tests de-skipped /
strengthened where they were still guarded.

Plan: `dev/plans/2026-06-10-p1-parity-backlog.md`. Version bumped to **0.4.4**.

## Done this slice (new code)

- **C1 - singleton-chain identity.** `chain_reader(list(d))` delegates to
  `read_only(d)`; `chain_writer(list(d))` with no name returns `d`;
  `read_only()` is the identity on an already-read-only daf unless a name
  forces a rewrap. `R/chain_daf.R`; mirrors `chains.jl:83-85,116-118` +
  `read_only.jl:52-60`. Unskipped `chains / one / reader|writer` and added
  the `read_only(chain)` idempotence assertions to `chains / two`.
  Commit `c582c2a`.
- **M2 - sparse-preserving collect-axis.** `MERGE_COLLECT_AXIS` vector merge
  now runs Julia's storage-savings heuristic
  (`sparse_vectors_storage_fraction`, default 0.25) and builds a `dgCMatrix`
  when sparse wins; strings/bit64 stay dense. `R/concat.R`
  (`.concat_collect_vector_matrix`, `.concat_stored_vector_nnz`,
  `.concat_eltype_bytes`, `.concat_indtype_bytes`); mirrors
  `concat.jl:949-1068`. The `sparse_if_saves_storage_fraction` kwarg, which
  was previously accepted-but-ignored, is now plumbed through `.concat_merge`.
  Commit `5c16976`.

## Verified already-implemented (no code, or test cleanup only)

- **R5 / R6 (reorder).** Multi-writer atomic reorder (R5) and memory_daf
  crash-recovery atomicity (R6) were already live and green: memory_daf
  snapshots before mutation and `format_reset_reorder` restores from it.
  The crash-recovery test helper carried a dead `can_recover = FALSE` branch
  with a stale comment ("memory reset is a no-op"); removed. Commit `d739e95`.
- **M1 (merge wildcards), M4 (prefixed override), M5 (memory_daf
  sparseVector).** All implemented in prior sessions. M5: memory_daf stores
  `Matrix::sparseVector` natively (raw storage stays a `dsparseVector`) and
  surfaces a named dense vector at `get_vector` - verified live. M1: the
  `.concat_expand_merge_wildcards` skeleton was already complete; the merge
  tests now use the real `ALL_VECTORS` / `ALL_SCALARS` constants instead of
  the explicit-key translation. M4: `.concat_axis_vector` already treats
  `prefixed=` as an override (no `do_prefix` gate). All 6 former
  M1/M2/M4/M5 skips in `test-concat-jl-parity.R` are gone (now 104 pass / 0 skip).
- **E3-E10 (queries).** Already implemented in the parser-strictness and
  later slices. The authoritative status line in
  `2026-05-03-queries-jl-parity-divergences.md` (the 2026-05-04 header) reads
  "**Open: E11 only**"; the per-id E3-E10 *detail sections* below it are
  stale (they predate the fixes). Confirmed by exercising the forms
  (e.g. E3 `@ cell [ UMIs @ gene = A > 0 ]`) - all pass.
  `test-queries-jl-parity.R` has only 2 skips, both the accepted T-class
  (E11/E1). The E-series feature test files (count-variants, mask-variants,
  compare-types, lookup-chains, matrix/group variants, axis-matrix-slice)
  are 148 pass / 0 skip.
- **V2 / V3 / V4 / V6 / V7 (views).** Wildcard-query validation (V2),
  strict include-list (V3), matrix eltwise (V4), bare `::` axis-prepend (V6),
  scalar-shape validation (V7) all already implemented and green.

## Not done

- **V5 (`__axis__` full-query substitution).** Attempted: make `__axis__`
  expand to the slot axis's full query body (mask included), matching
  `views.jl prepare_vector_query`. **Reverted** - it conflicts with dafr's
  view-as-subset architecture. dafr computes the override query over the
  *full* base axis and then subsets by the view's axis indices; self-masking
  the inner query makes the result shorter than the index vector, so the
  subset over-indexes (returns NA). The existing masked-`__axis__` test
  (`views / vector / masked / query`) passes because computing-then-subsetting
  is equivalent to filter-then-computing for *per-element* queries (the only
  kind currently expressible through the slot). The divergence is only
  observable for axis-population-dependent inner queries, which dafr's
  post-subset model cannot express without rearchitecting the view layer.
  Left as an accepted architectural divergence.
- **V1 (view tensors).** Deferred to its own slice. Genuine ~150-200 LOC
  feature (4-tuple viewer keys, `<entry>_<suffix>` matrix auto-grouping,
  `tensors:` description block, collection logic). One skip covering 8 Julia
  leaves; a Julia naming convention with low dafr usage. Not worth bundling
  with the small parity fixes here.
- **C3 (empty_*/builder API).** Out of this slice (the kickoff lists it under
  P1 but it is a cross-backend API: 8 format generics x 4 backends, and per
  the research the R value is semantic-only - no zero-copy benefit since R is
  copy-on-write). 10 tests across 3 files (`chains`, `contracts-access`,
  `data`) still skip on C3. Recommend its own slice with the C3 research
  report as the spec.
- **E1 / E11 (accepted).** Median-with-NaN (E1) and integer-reduction
  InexactError (E11) remain accepted divergences; untouched.
- **C2 (description `deep=`).** Out of scope; separate divergence.

## State

Full suite: **FAIL 0 | PASS 6086 | SKIP 133** (`NOT_CRAN=true devtools::test()`).
The 1 WARN is pre-existing and unrelated (scran `quickCluster` SVD on a small
matrix in `test-altrep-downstream.R`). Remaining skips are all expected:
c-blosc-not-built (zarr packed), CT3 (encode/decode_expression),
C2 (description deep), V1 (tensors), T-class (E1/E11), C3 (builder API).

## Remaining P1 after this slice

- C3 builder API (own slice).
- V1 view tensors (own slice).
- Dense parallel compute kernels (`% Abs`/`Round`/`Clamp`/`Convert`) - the
  perf lever, untouched here.
