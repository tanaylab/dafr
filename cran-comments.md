## Release summary

This is the first public release of `dafr` — a native R + C++ port of
`DataAxesFormats.jl` (Julia). It does not supersede an existing CRAN
package.

## Test environments

- Local R 4.4+ on Linux — PASS, 2616+ testthat assertions.

## R CMD check results

0 errors | 0 warnings | 4 notes.

Known NOTEs (non-blocking, will be addressed before CRAN submission):

- Non-standard top-level directory `benchmarks/` — developer
  benchmark runner; will be `.Rbuildignore`'d before submission.
- Hidden directory `.claude/` — developer-tool session state;
  already gitignored, pending `.Rbuildignore` entry.
- Installed package size (~6 MB) — compiled C++ kernels + fixtures;
  `strip` pass + fixture subset pending.
- "Unable to verify current time" — build-environment flake.

## Notes

This package exports 130+ user-facing functions, a pipe-composable
query DSL, and an AnnData facade. See `NEWS.md` for the full 0.1.0
feature list.
