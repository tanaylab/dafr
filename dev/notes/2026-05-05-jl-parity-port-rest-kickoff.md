# Kickoff — port the rest of DAF.jl's test suite

**Date:** 2026-05-05
**Predecessor:** `queries.jl` literal port + every E-class divergence
closed (see `dev/notes/2026-05-03-queries-jl-parity-divergences.md`,
post-Slice-3 status). The query DSL is at full Julia parity minus
E11 (kernel-level type-strictness, T-class).

## Why

`queries.jl` is one of 14 test files in `~/src/DataAxesFormats.jl/test/`.
For everything else (chains, views, contracts, concat, reorder,
operations, copies, computations, read_only, data, anndata,
cache_groups, reconstruction), dafr has its own R-side tests but
**no literal port** of the Julia suite. We have no signal whether
those areas behave the same as Julia on the cases the Julia tests
exercise.

The `queries.jl` port surfaced 13 closeable divergences that none of
dafr's existing query-DSL tests caught. Expect a similar yield from
the unported files: *most* assertions will pass, a handful will
surface real gaps (behaviour, error wording, named-result threading,
type strictness), and a few will be R-vs-Julia idiomatic divergences
to document and skip.

## Coverage map

Julia test files by size, with current dafr coverage:

| Julia file        | Lines | `@test` count | dafr-side ports                                |
|-------------------|------:|--------------:|------------------------------------------------|
| `data.jl`         |  4329 |          1160 | partial — 9 `test-files-*.R` + scattered tests |
| `contracts.jl`    |  1639 |           459 | partial — 4 `test-contracts*.R`                |
| `copies.jl`       |  1397 |           376 | partial — 8 `test-copies-*.R`                  |
| `read_only.jl`    |   764 |           399 | **none — no direct port**                      |
| `views.jl`        |   654 |           126 | partial — 5 `test-view*.R`                     |
| `operations.jl`   |   576 |           213 | partial — 5 `test-operations*.R`               |
| `chains.jl`       |   537 |           252 | partial — 3 `test-chain*.R`                    |
| `reorder.jl`      |   500 |           130 | partial — 4 `test-reorder*.R`                  |
| `concat.jl`       |   474 |           153 | partial — 1 `test-concat.R`                    |
| `computations.jl` |   434 |            58 | partial — 1 `test-computations.R`              |
| `cache_groups.jl` |   180 |            49 | partial — 1 `test-cache-group.R`               |
| `anndata.jl`      |   157 |            41 | partial — 3 `test-anndata*.R`                  |
| `reconstruction.jl` |  114 |           20 | partial — 1 `test-reconstruction.R`            |

(Excluded: `queries.jl` — already ported; `mmap_zip_stores.jl` —
covered by `test-mmap-zip-store-*.R`; `tokens.jl` — covered by
`test-query-tokens.R`; `adapters.jl` / `groups.jl` / `keys.jl` /
`registry.jl` — too small to warrant a slice.)

## Goal

Each unported file becomes a `tests/testthat/test-<name>-jl-parity.R`
that mirrors the Julia file's `nested_test` tree as
`test_that("<name> / <path>", { ... })`, following the
`test-queries-jl-parity.R` template. Divergences are documented in a
sister file `dev/notes/<date>-<name>-jl-parity-divergences.md`.

## Slicing

Six slices ordered by **size × isolation × risk**. Each is
independently shippable on `dev` and shippable to `main` once green.

### Slice A — `read_only.jl` — DROPPED on 2026-05-06

Originally framed as wrapper-mode enforcement (`ReadOnlyChainDaf`,
`ContractDaf` read-only, `ViewDaf` read-only, leaf storage read mode).
Reading the file end-to-end disproved that framing: `read_only.jl` is
purely an **array-primitives** test — `is_read_only_array`,
`read_only_array`, `copy_array`, and `brief()` formatting strings —
across `Vector|Matrix × Dense|Sparse × Named|Unnamed × {raw,
PermutedDimsArray(1,2), PermutedDimsArray(2,1), transpose, adjoint}`.
It never opens a Daf and never touches a wrapper.

R has no direct counterparts for the primitives this file tests
(`SparseArrays.ReadOnly` wrapper, `NamedArray` class, `PermutedDimsArray`
lazy view, distinct `transpose`/`adjoint`, `brief()` summaries). dafr
exposes none of these by name. A literal port would be ~390 test_that
blocks each skipping with "R has no SparseArrays.ReadOnly" — close to
zero yield.

The wrapper-mode behaviors the original framing was reaching for
(`ReadOnlyChainDaf`, `ContractDaf` read-only, `ViewDaf` read-only) live
in `chains.jl` (Slice B), `views.jl` / `contracts.jl` (Slice C), and
storage mode discipline lives in `data.jl` (Slice F). They will get
their literal ports there.

R-side read-only surface (altrep wrappers around mmap, immutable
results from `format_get_*`) is already covered by
`tests/testthat/test-altrep-*.R`.

### Slice B — `concat.jl` + `reorder.jl` + `chains.jl` (1511 lines) — NEXT

Mid-size, semantically related (multi-daf composition). All have
`test-concat.R` / `test-reorder*.R` / `test-chain*.R` companions
already, but the literal port will close gaps in:

- Empty / dataset-axis / dataset-property concat shapes (`concat.jl`).
- Reorder × wrapper combinations (`reorder.jl`).
- Empty-chain / unnamed-chain semantics (`chains.jl`).

Pick `concat.jl` first — it's where we already found a real bug last
slice (the `unname()` boundary in `.concat_axis_vector` /
`.concat_merge_vector`).

### Slice C — `views.jl` + `contracts.jl` (2293 lines)

Wrapper-API stress. `views.jl` covers axis renames, hidden vectors,
wildcard expansion, reduction-over-view; `contracts.jl` covers every
`required` × `optional` × `left` × `right` add/verify combination.
`contracts.jl` is large (1639 lines, 459 @tests) so consider splitting
into Slice C1 (views + contract-add) and Slice C2 (contract-verify
+ contract-as-reader).

### Slice D — `operations.jl` + `computations.jl` (1010 lines)

Algorithm-level tests: every reduction / eltwise op's per-type
behaviour (`operations.jl`) and the `@computation` API for
contract-defined functions (`computations.jl`). `operations.jl` will
likely surface T-class error-text divergences (R vs. Julia type
errors); `computations.jl` is mostly net-new for dafr.

### Slice E — `copies.jl` (1397 lines, 376 @tests)

Copy semantics — `copy_axis`, `copy_vector`, `copy_matrix`, plus the
empty / overwrite / view-source / chain-source cross-product. dafr
already has 8 `test-copies-*.R` files, so the yield is probably
modest; do this after the higher-risk slices.

### Slice F — `data.jl` (4329 lines, 1160 @tests)

The big one. Storage-API stress test: axes, vectors, matrices,
relayout, sparse, type coercion, error messages, every backend × every
mode combination. dafr's `test-files-*.R` / `test-memory-*.R` /
`test-zarr-*.R` already cover much of it; the literal port is mostly
about catching the long-tail divergences.

Two reasonable shapes:

1. Single big-bang port + divergence-catalog pass (same playbook as
   `queries.jl`). Risky branch life.
2. Split by section: e.g. F1 (axes), F2 (scalars + vectors), F3
   (matrices + relayout), F4 (errors + edge cases). Smaller PRs but
   slice-exit overhead × 4.

Recommend (2) once Slices A-E are done — by then we'll know the
divergence shape better.

### Slice G — small files (cleanup)

`cache_groups.jl` (180), `anndata.jl` (157), `reconstruction.jl` (114).
Three tiny files, fold into one slice. ~110 @tests total. Low risk,
mostly assertion-tightening on existing R-side tests.

## Methodology (per slice)

Same playbook as the `queries.jl`-parity port:

1. **Set up.** Branch off `dev` as `slice-<name>-jl-parity`. Read
   the Julia file end-to-end before touching anything.
2. **Translate the test tree.** Each `nested_test("foo") do ... end`
   becomes one `test_that("<file> / <path> / foo", { ... })`. Carry
   the Julia path verbatim into the test name. Setup is duplicated
   per leaf, matching Julia's fresh-state semantics.
3. **Run the suite.** Expect failures. For each:
   - **Behavior bug in dafr** → fix inline, commit with
     `fix(parity): <description>`.
   - **R-vs-Julia idiomatic divergence** that's text-only or
     R-fundamental (e.g. `'x' must be numeric` vs. Julia's typed
     error, integer auto-promotion in `Sum`) → `skip("<id>: <reason>")`
     with a sharp message; record in the divergence note.
   - **Test-side error in the port** (typo, wrong setup) → fix the
     test.
4. **Catalogue.** Maintain
   `dev/notes/<date>-<name>-jl-parity-divergences.md` with one
   numbered entry per skip. Format mirrors
   `2026-05-03-queries-jl-parity-divergences.md`.
5. **Exit.** Slice exit note + `ship.sh` to main, same as the four
   already-shipped parity slices.

## Acceptance

Per slice: `FAIL 0 | PASS ≥ <ported test count>`, with every skip
keyed to a divergence-note ID. No skip should read just
`# TODO`.

End-state goal (after Slices A-G): every Julia test file has a
sister `test-<name>-jl-parity.R` running on every PR, every E-class
behaviour divergence is closed, every T-class is documented and
skipped with reasoning. Same posture we have for `queries.jl` today.

## Risks / scope cuts

- **Branch life.** Slices C and F are big enough to drag if scope
  isn't held. Pre-commit budget: 2 weeks for C, 3-4 weeks for F.
  If a slice runs over, split rather than pile on.
- **Test runtime.** dafr's full suite is at 4626 PASS today and
  takes ~3 min on Linux. Adding ~3000 ported tests roughly doubles
  it. Keep an eye on CI duration — if it crosses 15 min on a single
  matrix entry, parallelise via testthat parallel execution rather
  than dropping coverage.
- **Yield uncertainty.** `queries.jl` had a 13-divergence yield (8
  closed, 1 deferred, 4 T-class). Other files might have less (the
  parity work has already lifted format-API contracts that were the
  source of half the queries.jl gaps). Don't get discouraged if a
  slice ports cleanly — that's the expected outcome for ~half of
  them.
- **Don't gold-plate.** If a slice's `test-<name>-jl-parity.R`
  catches no new bugs and adds no new skips, ship it as a
  regression-guard slice with the test count as the win. We don't
  need to find bugs to justify the port; the literal-equivalence
  guarantee is the win.

## Out of scope for this kickoff

- Closing E11 (kernel-level int-promotion type-strictness). Filed
  in `2026-05-03-queries-jl-parity-divergences.md` as the only open
  E-class item; would need a reduction-kernel rewrite. Revisit if/when
  there's an unrelated kernel touch.
- Closing B8 (introspection lenience on partial queries). Harmless
  per the existing exit note; no test gates on it.
- New features (compute helpers, h5ad ergonomics, etc.). This
  kickoff is parity-coverage only.
