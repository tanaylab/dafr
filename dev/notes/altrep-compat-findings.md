# ALTREP compatibility findings — 2026-04-19

## Environment

- R version: R version 4.4.1 (2024-06-14)
- Matrix version: 1.7.3
- Seurat version: 5.1.0
- scran version: 1.32.0
- SingleCellExperiment version: 1.26.0

## Results

| Package | Installed | Test | Result | Notes |
|---|---|---|---|---|
| Seurat  | yes | CreateSeuratObject | pass  | Object builds correctly; `ncol(obj) == 100`. Internally materializes the counts slot (ALTREP lost on stored copy), but the user-supplied mmap matrix `m` itself retains ALTREP. |
| scran   | yes | quickCluster       | pass  | Returns a 100-element clustering (3 clusters). ALTREP is preserved through `SingleCellExperiment(assays = list(counts = m))` wrapping; emits a harmless `irlba` warning about SVD size on this tiny fixture. |

Both tests passed. Testthat summary: `[ FAIL 0 | WARN 1 | SKIP 0 | PASS 3 ]` (the third PASS is the `expect_length` assertion inside the scran test; the WARN is the upstream `irlba` SVD-size message, not an ALTREP issue).

## Detailed findings

### Seurat::CreateSeuratObject

- What the test did: constructed a 200 x 100 mmap-backed `dgCMatrix` with 10% density of integer-like counts and passed it as `counts =` to `CreateSeuratObject`.
- What happened: object was built successfully (`expect_s4_class(obj, "Seurat")` and `expect_equal(ncol(obj), 100)` both pass).
- ALTREP observation (from diagnostic run outside the test file):
  - Input `m@x`, `m@i`, `m@p` are ALTREP before the call and remain ALTREP after the call (no mutation of the user-supplied object).
  - The `counts` slot stored *inside* the Seurat object (`GetAssayData(obj, layer = "counts")`) is a regular `dgCMatrix` whose `@x/@i/@p` are NOT ALTREP. Seurat 5.1 copies / normalizes the counts matrix during `CreateSeuratObject` (likely through `as(..., "dgCMatrix")` or an explicit row/col-name and validity pipeline) and that copy materializes the ALTREP slots to plain R vectors.
  - Consequence: the mmap benefit is retained only if the user keeps a handle to the original matrix or operates on it before handing it to Seurat. The moment it enters a Seurat object the backing storage for *that copy* is a plain heap allocation.

### scran::quickCluster

- What the test did: wrapped the same kind of mmap-backed `dgCMatrix` in a `SingleCellExperiment` and called `scran::quickCluster(sce, min.size = 10)`.
- What happened: returned a factor of length 100 with 3 levels. Test assertion (`expect_length(cl, 100)`) passes.
- ALTREP observation:
  - `SingleCellExperiment(assays = list(counts = m))` preserves ALTREP — `assay(sce, "counts")@x/@i/@p` are all still ALTREP after wrapping.
  - The user-supplied `m` also retains ALTREP after `quickCluster` returns. `quickCluster` / beachmat / BiocSingular evidently read through DATAPTR without asserting it is a heap pointer and without mutating the input.
  - An `irlba` warning appears: `"You're computing too large a percentage of total singular values, use a standard svd instead."` This is purely an artifact of the 200 x 100 fixture being small relative to the default `d = 50` in the denoised-PCA step. It is unrelated to ALTREP.

## Mitigation decisions

### Seurat — copy/materialize on ingest

Classification: **Acceptable**.

Rationale: `CreateSeuratObject` succeeds and produces a correct object. The implicit materialization happens exactly once, at the boundary where the user has explicitly handed the matrix off to Seurat. Users who want to preserve mmap semantics for downstream Seurat analysis would have needed to materialize anyway (Seurat's internal pipeline — `NormalizeData`, `ScaleData`, etc. — is write-heavy). Document the behavior: "Seurat 5.x copies the counts matrix at ingest; the mmap benefit applies to whatever the user does *before* calling `CreateSeuratObject`, not to operations on the Seurat object itself." No code change needed on our side.

### scran — clean passthrough

Classification: **Acceptable / Better than expected**.

Rationale: `quickCluster` runs to completion, ALTREP is preserved end-to-end, and the fixture input is not mutated. This is the best possible outcome. No mitigation required.

## Conclusion

The POC gate remains **open**. The architectural bet — ALTREP-backed slots flowing from a `FilesDaf` into `dgCMatrix` and then into downstream single-cell tooling — is sound. Neither Seurat 5.1 nor scran 1.32 crashes, errors, or bypasses ALTREP in a way that violates our assumptions. Seurat materializes on ingest (acceptable and arguably necessary given its mutation-heavy pipeline); scran and the wider Bioconductor stack (SingleCellExperiment, beachmat, BiocSingular, irlba) handle ALTREP `dgCMatrix` slots transparently. We can proceed with the Slice 0 plan without redesign.
