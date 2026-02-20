# Install Julia packages needed for DataAxesFormats and TanayLabUtilities

Install Julia packages needed for DataAxesFormats and TanayLabUtilities

## Usage

``` r
install_daf_packages(force_dev = FALSE, confirm_install = NULL)
```

## Arguments

- force_dev:

  (Default=FALSE) Whether to force dev versions of packages

- confirm_install:

  Whether to allow installation of Julia packages that may write to the
  Julia package store. If `NULL` (default), prompt interactively and
  fail in non-interactive sessions.

## Value

No return value, called for side effects.
