## Release summary

`dafr` is the first public release of a native R + C++ implementation
of the DataAxesFormats (DAF) data model, originally implemented in Julia
(`DataAxesFormats.jl`). The R port provides MemoryDaf and FilesDaf
storage backends, a query DSL in both string and pipe-chain builder
form, read-only h5ad interop, contracts, views, and a dplyr-backed
tidy interface. This is **not** a revision of an existing CRAN package.

## Test environments

- Local: Linux (x86_64, R 4.4+, gcc 12) — PASS
- GitHub Actions:
  - ubuntu-latest / R release — PASS
  - ubuntu-latest / R devel — PASS (as of submission)
  - ubuntu-latest / R oldrel-1 — PASS
  - macos-latest / R release — PASS
  - windows-latest / R release — PASS

Full test suite: 2800+ testthat assertions across 45 test files.

## R CMD check results

0 errors | 0 warnings | NOTEs below.

### Installed package size

Under the 5 MB target after fixture compression on CRAN's stripped
install (compiled library strips from 3.5 MB to ~220 KB). Compiled
libs before strip are ~3.4 MB (OpenMP-parallel C++ kernels for
reductions, group-by, sparse I/O). Post-strip total sits comfortably
under the CRAN 5 MB limit.

### Non-standard files

None in the built tarball; development-only directories
(`dev/`, `benchmarks/`, `.claude/`, `.worktrees/`) are excluded via
`.Rbuildignore`.

## Downstream dependencies

None. This is a new package with no reverse dependencies on CRAN.

## Additional notes

- C++17 is declared in `SystemRequirements`. All compilation flags come
  from R's build system (no hard-coded flags beyond `PKG_CXXFLAGS` and
  `PKG_LIBS` pulling in `$(SHLIB_OPENMP_CXXFLAGS)`, `$(LAPACK_LIBS)`,
  `$(BLAS_LIBS)`, `$(FLIBS)`).
- Several examples are wrapped in `\dontrun{}` because they require
  on-disk FilesDaf stores, h5ad files, or are tied to benchmark-scale
  data. All cheap examples run by default and pass under `--run-donttest`.
- `hdf5r` is in Suggests (gated via `rlang::check_installed()` at each
  h5ad entry point). Core Daf functionality has no HDF5 dependency.

## Example runtime

`devtools::run_examples(run_dontrun = FALSE, run_donttest = TRUE)`
completes in well under the 5-minute CRAN ceiling on the developer
machine. Individual examples that load bundled fixtures are kept
fast by design (compressed RDS fixtures, no network, no disk writes);
any example whose runtime approaches 5 s is wrapped in `\donttest{}`.
