# Set up of the Julia environment needed for DataAxesFormats and TanayLabUtilities

This will set up a new Julia environment in the current working
directory or another folder if provided. This environment will then be
set with all Julia dependencies needed.

## Usage

``` r
setup_daf(
  pkg_check = TRUE,
  seed = NULL,
  env_path = getwd(),
  installJulia = FALSE,
  force_dev = getOption("dafr.force_dev", FALSE),
  confirm_install = NULL,
  level = "Warn",
  show_time = TRUE,
  show_module = TRUE,
  show_location = FALSE,
  julia_environment = getOption("dafr.julia_environment", "default"),
  JULIA_HOME = getOption("dafr.JULIA_HOME", NULL),
  ...
)
```

## Arguments

- pkg_check:

  (Default=TRUE) Check whether needed Julia packages are installed.

- seed:

  Seed to be used.

- env_path:

  The path to were the Julia environment should be created. By default,
  this is the current working directory.

- installJulia:

  (Default=TRUE) Whether to install Julia

- force_dev:

  (Default=FALSE) Whether to force dev versions of packages, default
  value comes from getOption("dafr.force_dev")

- confirm_install:

  Whether to allow installation of Julia packages that may write to the
  Julia package store. If `NULL` (default), prompt interactively and
  fail in non-interactive sessions.

- level:

  Log level, one of "Debug", "Info", "Warn", "Error", or an integer
  (default: "Warn")

- show_time:

  Whether to show timestamp (default: TRUE)

- show_module:

  Whether to show module name (default: TRUE)

- show_location:

  Whether to show source location (default: FALSE)

- julia_environment:

  Specify which Julia environment to use: "custom" creates a new
  environment (default if option not set), "default" uses the default
  Julia environment, any other value can be the path to a custom
  environment. Default value comes from
  getOption("dafr.julia_environment")

- JULIA_HOME:

  The path to the Julia installation. Default value comes from
  getOption("dafr.JULIA_HOME"). See
  [`julia_setup`](https://rdrr.io/pkg/JuliaCall/man/julia_setup.html)
  for more details.

- ...:

  Other parameters passed on to
  [`julia_setup`](https://rdrr.io/pkg/JuliaCall/man/julia_setup.html)

## Value

No return value, called for side effects.

## Examples

``` r
if (FALSE) { # \dontrun{
# Install from default URLs
setup_daf()

# Time consuming and requires Julia
setup_daf(installJulia = TRUE, seed = 60427)

# Install from latest github versions
setup_daf(
    installJulia = TRUE,
    custom_urls = c(
        "https://github.com/tanaylab/DataAxesFormats.jl",
        "https://github.com/tanaylab/TanayLabUtilities.jl"
    )
)

# Use default Julia environment instead of creating a new one
setup_daf(julia_environment = "default")

# Only load packages without installation
setup_daf(pkg_check = FALSE)

# Set global option
options(dafr.julia_environment = "default")
setup_daf() # Will use the default Julia environment
} # }
```
