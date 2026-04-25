## Test environments

- Local: Linux (x86_64, R 4.4, gcc 13) — 0 errors, 0 warnings, 2 notes.
- GitHub Actions (R-CMD-check.yaml matrix):
  - ubuntu-latest on R r-devel
  - ubuntu-latest on R r-release
  - ubuntu-latest on R r-oldrel-1
  - macos-latest on R r-release
  - windows-latest on R r-release
- R-hub v2 (run cayenned-slothbear, branch `main`):
  - linux (R-devel), windows (R-devel), macos-arm64 (R-devel),
    gcc14, atlas, valgrind — all `Status: OK`.

Full `testthat` suite: ~2987 assertions.

## R-hub notes

- **valgrind.** Reports 432 bytes "possibly lost" in a single block,
  inside glibc's pthread thread-local-storage allocator
  (`_dl_allocate_tls` invoked from `pthread_create` by `GOMP_parallel`
  on the first OpenMP kernel call). `definitely lost` and
  `indirectly lost` are both 0 bytes. This is the universal OpenMP
  false-positive seen on every CRAN package that uses libgomp; it is
  not a leak in our code.

- **nold / clang-asan / clang-ubsan.** Could not be checked against
  the full Suggests stack on R 4.7-devel because two transitive
  Suggests deps fail to build there: `RcppAnnoy` (via `Seurat`) and
  `RcppTOML` (via `reticulate`) reference the now-removed C symbol
  `R_NamespaceRegistry` in `Rcpp/Function.h`. These are upstream
  packages we don't depend on directly. A targeted probe with those
  Suggests removed (R-hub run undiseased-polecat) confirmed the
  package itself is clean under clang-ASAN and clang-UBSAN
  (32m and 36m runs respectively, 0 sanitizer findings).

## R CMD check notes worth justifying

- **Thread compliance.** `.onLoad()` detects CRAN's check harness
  (via `_R_CHECK_LIMIT_CORES_` or `OMP_THREAD_LIMIT <= 2`) and caps
  OpenMP threads to 2 via `set_num_threads()`. Users can override
  with `options(dafr.num_threads = N)` or `set_num_threads(N)`.
  Examples, tests, and vignettes all respect the 2-core policy.
- **`hdf5r` in Suggests.** Gated via `rlang::check_installed()` at
  each h5ad entry point. Core Daf functionality has no HDF5 dependency.

## Downstream dependencies

None — new package.
