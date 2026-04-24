## Test environments

- Local: Linux (x86_64, R 4.4, gcc 13) — 0 errors, 0 warnings, 2 notes.
- GitHub Actions (R-CMD-check.yaml matrix):
  - ubuntu-latest on R r-devel
  - ubuntu-latest on R r-release
  - ubuntu-latest on R r-oldrel-1
  - macos-latest on R r-release
  - windows-latest on R r-release

Full `testthat` suite: 2913 assertions.

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
