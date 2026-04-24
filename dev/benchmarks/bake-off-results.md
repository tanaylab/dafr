# Slice 0 bake-off results — 2026-04-19

Machine: AlmaLinux 8.10 (Cerulean Leopard), 128 cores, ~1 TiB RAM.
BLAS/LAPACK: MKL 2024.1 (`libmkl_gf_lp64.so.2`), LAPACK 3.11.0.
R version 4.4.1.

Ran in SMALL=1 mode (portable, small sizes):
- log(x)+y: n = 10M doubles
- CSC colSums: 1000 × 10000, 1% density
- CSC→CSR transpose: 2000 × 2000, 5% density

## Measurements

| Kernel | D (cpp11+BLAS) median | B (RcppEigen) median | Ratio D/B | Winner |
|---|---|---|---|---|
| log(x) + y | 12.4 ms | 126.0 ms | 0.098 | **D** |
| CSC colSums | 136 µs | 136 µs | 1.006 | tie |
| CSC→CSR transpose | 1.83 ms | 1.21 ms | 1.508 | **B** |

Ratio D/B < 1 means D is faster; > 1 means B is faster.

## Decision

Stick with D for log(x)+y and colSums — D wins by 10× on log_add and ties on colSums.

**Reopen for CSC→CSR transpose — B wins by ~51% (D/B = 1.508), well above the 20% threshold.**

The transpose kernel gap is structurally plausible: Eigen's `SparseMatrix<RowMajor>` assignment from a ColMajor matrix is a single in-place re-index, whereas the cpp11 D-arm does explicit pointer arithmetic with a counting-sort pass. At SMALL=1 sizes the absolute times are both sub-2ms, so the relative gap is not a latency concern for typical use, but it will scale with nnz. Full-size measurement (100K×100K, 5% density, ~500M nnz) is needed to decide whether to swap the transpose kernel to Eigen or tune the D-arm algorithm.

## Follow-ups

- **Full-size re-run for transpose**: 100K×100K @ 5% nnz is ~500M entries. At that scale the D/B ratio may narrow (cache effects differ at scale) or widen. Defer decision on swapping the transpose kernel until that run completes.
- **SMALL=1 colSums is inconclusive at small size**: 1K×10K @ 1% is only 100K nnz. The tie (D/B = 1.006) should be verified at 100K×1M scale before treating it as settled.
- **MKL thread count**: MKL was linked but the dense log_add kernel does not use it (element-wise loop); the 10× D advantage there is inherent to the algorithm (no Eigen overhead), not BLAS threads. OpenMP thread count was not pinned; the 128-core machine may allow MKL to use many threads for other ops.
- **Eigen OpenMP**: RcppEigen does not enable Eigen's OpenMP parallelism by default (no `-fopenmp` in the scratch package's `Makevars`), so B-arm colSums and transpose times are single-threaded. If OpenMP were enabled for B, the transpose gap could widen further.
