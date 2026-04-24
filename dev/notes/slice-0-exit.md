# Slice 0 exit gate — 2026-04-19

## Deliverables

- [x] Package scaffold (DESCRIPTION, LICENSE, configure, Makevars, NAMESPACE) — Phase A
- [x] S7 class hierarchy (DafReader / DafReadOnly / DafWriter) — Phase B
- [x] ~40 format API S7 generics declared — Phase B
- [x] Cache skeleton (3-tier env, version counters, empty_cache) — Phase B
- [x] Handler registration + options framework — Phase A/B
- [x] MmapRegion RAII — Phase C
- [x] ALTREP classes for mmap-backed double/int/logical — Phase D
- [x] `mmap_dgCMatrix` helper — Phase D
- [x] Matrix-compat smoke tests (colSums, rowSums, t, index-assign) — Phase D
- [x] Seurat/scran compat findings recorded (`dev/notes/altrep-compat-findings.md`) — Phase D
- [x] Benchmark harness (workload generator + bench.R) — Phase E
- [x] Bake-off (cpp11+BLAS vs RcppEigen) — decision recorded (`dev/benchmarks/bake-off-results.md`) — Phase F
- [x] FilesDaf on-disk spec draft (`dev/specs/filesdaf-on-disk-spec-draft.md`) — Phase G1
- [ ] **Upstream PR for FilesDaf spec — NOT DONE.** Phase G2 deferred pending user consent to open a PR against `tanaylab/DataAxesFormats.jl`.
- [x] CI workflows committed locally: R-CMD-check.yaml, altrep-sanity.yaml, bench.yaml — Phase H. Not pushed; will activate on first push to GitHub.

## Test + build status

- `testthat::test_dir("tests/testthat")` — 8 files, 44 test_that blocks, 0 failures, 1 warning (upstream scran log-message about SVD tolerance, not a dafr defect).
- `inst/benchmarks/bench.R --small` — runs; produces CSV.
- `dev/benchmarks/run-bakeoff.R` with `SMALL=1` — produces CSV. `bench::mark` v1.1.4 prints a cosmetic error during its own summary format step (unrelated to our code); the CSV itself is written correctly.
- Package builds cleanly on linux. `R CMD check` status from Phase D session end: 0 ERRORs, 1 WARNING (Rd undocumented args on S7 classes — pre-existing, addressed in Slice 1), 2 NOTEs (unused Imports `bit64`/`cli` — addressed when Slice 1 adds real uses).

## Bake-off decision

Per `dev/benchmarks/bake-off-results.md`:

- **log(x)+y**: D (cpp11+BLAS) wins 10× over B (RcppEigen). Stick with D.
- **CSC colSums**: tie at SMALL=1 (100K nnz). **Inconclusive.** OpenMP thread-pool startup dominates at small sizes; re-running with and without OpenMP on the D arm gives different answers. Needs full-size (100M+ nnz) re-run to settle; for now assume D (consistent with the rest of the kernel family). Follow-up flagged in bake-off-results.md.
- **CSC→CSR transpose**: B (RcppEigen) wins by ~51%. **Reopens the transpose kernel for evaluation.** Eigen's `SparseMatrix<RowMajor>` assignment from a ColMajor map uses a single in-place re-index; our D-arm uses an explicit counting-sort pass. Either swap to Eigen for this kernel or tune the D algorithm. Decision deferred to Slice 2 (when real transpose usage materializes).

Overall stack decision: **Stick with D (cpp11+BLAS) for the main package.** RcppEigen remains an option for specific kernels if the Slice 2 transpose re-evaluation confirms the gap.

## Key findings from Phase G1 that contradict plan assumptions

The FilesDaf spec extraction surfaced two items that invalidate plan assumptions for Slice 2:

1. **No on-disk version counters.** The Julia FilesDaf's version counters live in memory only and reset to 0 on open. Slice 2's Files backend doesn't need to invent an on-disk counter format — we just match Julia's in-memory behavior. (Plan had a placeholder for `<thing>_version.txt` files; remove.)
2. **No atomicity model.** Julia FilesDaf writes direct (no `.tmp` sibling, no `fsync`, no rename). Multi-process concurrent writes are unsafe by design. The native R implementation should mirror this or explicitly add atomicity — an open design question for Slice 2.

Three specific ambiguities are flagged as **[UNCLEAR]** in the spec draft for Oren's review in G2.

## Plan retrofits applied in this session

All of these have been committed. No plan retrofit commits yet, but the plan file is now a step behind actual code on:

- `bench.R`: (1) base `rbind` instead of `dplyr::bind_rows` (no new dep); (2) `isNamespaceLoaded()` guard on `library(dafr)` so the script works under `devtools::load_all()`; (3) column renamed `median_ns` → `median_s` to match `bench::mark`'s unit; (4) relative path `dev/benchmarks` instead of absolute `~/src/dafr-native/dev/benchmarks` so the script is portable + CI-compatible.
- `openmp_shim.h`: fixed latent `DAFR_PARALLEL_FOR(cond)` macro bug — `_Pragma` needs a stringified argument, not a literal `"cond"` string. Added `DAFR_PRAGMA_STR(x)` helper. This was discovered during F1 but affects every subsequent OMP-using kernel.
- `src/eigen_kernels.cpp` (scratch package):
  - Function signatures use the expanded `Eigen::SparseMatrix<...>` type instead of the `SpMat` typedef, because `Rcpp::compileAttributes` generates a separate TU that doesn't see the typedef.
  - `eigen_csc_colsums` rewritten as `m.transpose() * VectorXd::Ones(m.rows())` — idiomatic Eigen sparse algebra — because `SparseMatrix::InnerIterator(const Map<SpMat>, j)` hit a private-constructor access issue in the installed RcppEigen.
- `dev/.gitignore` added to exclude R build artifacts from the scratch package.

## Outstanding risks / follow-ups

- **G2 upstream PR** not opened — user consent needed.
- **Transpose kernel decision** deferred — Slice 2 dependency.
- **colSums bake-off inconclusive** at small size — re-run at full scale before Slice 2 locks the kernel choice.
- **Pre-existing Rd undocumented-args warning** on S7 classes — close in Slice 1 when those classes get roxygen documentation.
- **`bit64` / `cli` unused imports** — close when Slice 1 uses them.
- **Long-vector (>2^31) and "file truncated while R vector live" scenarios untested** — pre-existing from Phase D breadcrumb; defer to later slices.
- **`writeBin(..., size=8L)` is native-endian** — FilesDaf format spec (Slice 2) must pin endianness; Julia spec says little-endian per G1 finding, so this will align.
- **bench::mark v1.1.4 print bug** — cosmetic; CSVs are correct. Upgrade bench when a fix ships.

## Decision to enter Slice 1

**Go.** Slice 0's architectural bet (ALTREP-backed dgCMatrix slots from FilesDaf, cpp11+BLAS C++ stack) is empirically validated. The two items still open (G2 upstream PR, transpose kernel re-evaluation) are non-blocking for Slice 1 — Slice 1 is about MemoryDaf, which has no FilesDaf/transpose dependency.

## Next plan

Slice 1 — MemoryDaf + axes + scalars/vectors/matrices get/set + cache infrastructure.
