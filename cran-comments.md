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

Julia-dependent tests are skipped on CRAN (`tests/testthat.R`) because they require an external
Julia runtime and Julia package installation/downloads. CRAN checks therefore run package load,
documentation, examples, and non-Julia checks without external runtime/network setup.

To avoid unintended writes in non-interactive environments, Julia package installation now requires
explicit confirmation (`confirm_install = TRUE`) or is skipped (`pkg_check = FALSE`).
