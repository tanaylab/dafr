#' Install Julia packages if needed
#'
#' @param ... strings of package names
#'
#' @noRd
install_pkg <- function(...) {
    for (pkg in as.character(list(...))) {
        JuliaCall::julia_install_package_if_needed(pkg)
    }
}

#' Obtain the status of the current Julia project
#'
#' @noRd
julia_project_status <- function() {
    JuliaCall::julia_command("Pkg.status()")
}

#' Loads Julia packages
#'
#' @param ... strings of package names
#'
#' @noRd
using_packages <- function(...) {
    for (pkg in list(...)) {
        JuliaCall::julia_library(pkg)
    }
}

#' Install Julia packages needed for DataAxesFormats and TanayLabUtilities
#'
#' @param force_dev (Default=FALSE) Whether to force dev versions of packages
#'
#' @return No return value, called for side effects.
#' @keywords internal
#' @export
install_daf_packages <- function(force_dev = FALSE) {
    JuliaCall::julia_library("Pkg")

    pkgs_needed <- list("TanayLabUtilities", "DataAxesFormats", "Logging")
    if (force_dev) {
        do.call(install_pkg, list(
            "https://github.com/tanaylab/TanayLabUtilities.jl",
            "https://github.com/tanaylab/DataAxesFormats.jl",
            "Logging"
        ))
    } else {
        do.call(install_pkg, pkgs_needed)
    }

    other_pkgs <- list("DataFrames", "HDF5", "LinearAlgebra", "Muon", "NamedArrays", "SparseArrays")
    do.call(install_pkg, other_pkgs)
}

#' Load Julia packages needed for DataAxesFormats and TanayLabUtilities
#'
#' @return No return value, called for side effects.
#' @keywords internal
#' @export
load_daf_packages <- function() {
    pkgs_needed <- list("TanayLabUtilities", "DataAxesFormats", "Logging")
    do.call(using_packages, pkgs_needed)

    import_julia_packages()
    define_julia_functions()
}

#' Set up of the Julia environment needed for DataAxesFormats and TanayLabUtilities
#'
#' This will set up a new Julia environment in the current working
#' directory or another folder if provided. This environment will
#' then be set with all Julia dependencies needed.
#'
#' @param pkg_check (Default=TRUE) Check whether needed Julia packages
#'                  are installed.
#' @param seed Seed to be used.
#' @param env_path The path to were the Julia environment should be created.
#'                 By default, this is the current working directory.
#' @param installJulia (Default=TRUE) Whether to install Julia
#' @param force_dev (Default=FALSE) Whether to force dev versions of packages, default value comes from getOption("dafr.force_dev")
#' @param julia_environment Specify which Julia environment to use:
#'                         "custom" creates a new environment (default if option not set),
#'                         "default" uses the default Julia environment,
#'                         any other value can be the path to a custom environment.
#'                         Default value comes from getOption("dafr.julia_environment")
#' @param JULIA_HOME The path to the Julia installation.
#'                    Default value comes from getOption("dafr.JULIA_HOME").
#'                    See \code{\link[JuliaCall]{julia_setup}} for more details.
#' @param ... Other parameters passed on to \code{\link[JuliaCall]{julia_setup}}
#'
#' @return No return value, called for side effects.
#' @examples
#' \dontrun{
#' # Install from default URLs
#' setup_daf()
#'
#' # Time consuming and requires Julia
#' setup_daf(installJulia = TRUE, seed = 60427)
#'
#' # Install from latest github versions
#' setup_daf(
#'     installJulia = TRUE,
#'     custom_urls = c(
#'         "https://github.com/tanaylab/DataAxesFormats.jl",
#'         "https://github.com/tanaylab/TanayLabUtilities.jl"
#'     )
#' )
#'
#' # Use default Julia environment instead of creating a new one
#' setup_daf(julia_environment = "default")
#'
#' # Only load packages without installation
#' setup_daf(pkg_check = FALSE)
#'
#' # Set global option
#' options(dafr.julia_environment = "default")
#' setup_daf() # Will use the default Julia environment
#' }
#' @inheritParams setup_logger
#' @export
setup_daf <- function(pkg_check = TRUE, seed = NULL,
                      env_path = getwd(), installJulia = FALSE,
                      force_dev = getOption("dafr.force_dev", FALSE),
                      level = "Warn",
                      show_time = TRUE,
                      show_module = TRUE,
                      show_location = FALSE,
                      julia_environment = getOption("dafr.julia_environment", "default"),
                      JULIA_HOME = getOption("dafr.JULIA_HOME", NULL),
                      ...) {
    julia <- JuliaCall::julia_setup(installJulia = installJulia, JULIA_HOME = JULIA_HOME, ...)
    JuliaCall::julia_library("Pkg")

    # Use default Julia environment if specified
    if (julia_environment != "custom") {
        use_default_julia_environment(julia_environment)
    }

    # Install packages if required
    if (pkg_check) {
        install_daf_packages(force_dev = force_dev)
    }

    # Load packages
    load_daf_packages()

    # Set seed if provided
    if (!is.null(seed)) {
        set_seed(seed)
    }

    setup_logger(
        level = level, show_time = show_time,
        show_module = show_module, show_location = show_location
    )

    cli::cli_alert_info("{.strong DataAxesFormats.jl} and {.strong TanayLabUtilities.jl} environment setup complete")
}
