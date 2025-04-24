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

#' Set up of the Julia environment needed for DataAxesFormats and TanayLabUtilities
#'
#' This will set up a new Julia environment in the current working
#' directory or another folder if provided. This environment will
#' then be set with all Julia dependencies needed.
#'
#' @param pkg_check (Default=TRUE) Check whether needed Julia packages
#'                  are installed
#' @param seed Seed to be used.
#' @param env_path The path to were the Julia environment should be created.
#'                 By default, this is the current working directory.
#' @param installJulia (Default=TRUE) Whether to install Julia
#' @param force_dev (Default=FALSE) Whether to force dev versions of packages
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
#' }
#' @inheritParams setup_logger
#' @export
setup_daf <- function(pkg_check = TRUE, seed = NULL,
                      env_path = getwd(), installJulia = FALSE,
                      force_dev = TRUE, level = "Warn",
                      show_time = TRUE, show_module = TRUE,
                      show_location = FALSE, ...) {
    julia <- JuliaCall::julia_setup(installJulia = installJulia, ...)
    JuliaCall::julia_library("Pkg")

    pkgs_needed <- list("TanayLabUtilities", "DataAxesFormats", "Logging")
    if (pkg_check) {
        if (force_dev) {
            do.call(install_pkg, list(
                "https://github.com/tanaylab/TanayLabUtilities.jl",
                "https://github.com/tanaylab/DataAxesFormats.jl",
                "Logging"
            ))
        } else {
            do.call(install_pkg, pkgs_needed)
        }
    }

    other_pkgs <- list("DataFrames", "HDF5", "LinearAlgebra", "Muon", "NamedArrays", "SparseArrays")
    do.call(install_pkg, other_pkgs)

    # Load packages
    do.call(using_packages, pkgs_needed)

    # Set seed if provided
    if (!is.null(seed)) {
        set_seed(seed)
    }

    setup_logger(
        level = level, show_time = show_time,
        show_module = show_module, show_location = show_location
    )

    import_julia_packages()
    define_julia_functions()

    cli::cli_alert_info("{.strong DataAxesFormats.jl} and {.strong TanayLabUtilities.jl} environment setup complete")
}
