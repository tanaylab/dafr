#' Set a seed both in Julia and R
#'
#' @param seed seed to be used
#'
#' @return No return value, called for side effects.
#' @examples
#' \dontrun{
#' ## Needs previous call to `setup_daf` which is time
#' ## consuming and requires Julia
#' setup_daf(installJulia = TRUE, seed = 123)
#' set_seed(123)
#' }
#' @export
set_seed <- function(seed) {
    JuliaCall::julia_command(paste0("Random.seed!(", seed, ");"))
    set.seed(seed)
    cli::cli_alert_info("Set the seed of Julia and R to {.val {seed}}")
}

#' Get a random symbol name for Julia variables.
#' taken from BayesFluxR
#'
#' @return A random symbol string
#'
#' @noRd
get_random_symbol <- function() {
    nframe <- sys.nframe()
    caller <- ""
    if (nframe > 1) {
        caller <- deparse(sys.calls()[[nframe - 1]])
        caller <- strsplit(caller, "\\(")[[1]][1]
        caller <- gsub("\\.", "_", caller)
        caller <- paste0(caller, "_")
    }
    sym <- paste0(sample(letters, 5, replace = TRUE), collapse = "")
    paste0(caller, sym)
}

#' Null coalescing operator from R 4.1+, backported for compatibility
#'
#' @param x First value
#' @param y Second value (used if x is NULL)
#' @return x if not NULL, otherwise y
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}
