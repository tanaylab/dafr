## Test environments

- Local Linux (AlmaLinux 8.10), R 4.4.1

## R CMD check results

- `R CMD build .`
- `_R_CHECK_SYSTEM_CLOCK_=FALSE R CMD check --as-cran --no-manual dafr_0.0.3.tar.gz`

Result: 0 errors | 0 warnings | 1 note

Notes observed:
- New submission.

## Julia dependency notes

`dafr` interfaces with Julia via `JuliaCall`. Julia runtime and Julia packages are declared in
`SystemRequirements` and are only initialized when users call `setup_daf()`.

The test suite is gated to avoid Julia-dependent checks on CRAN environments.
