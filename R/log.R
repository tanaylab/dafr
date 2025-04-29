#' Set up a global logger
#'
#' Setup a global logger that will print into a specified output stream (stdout or stderr),
#' with configurable options for timestamps and other information.
#'
#' @param io Output stream, either stdout or stderr (default: stderr)
#' @param level Log level, one of "Debug", "Info", "Warn", "Error", or an integer (default: "Warn")
#' @param show_time Whether to show timestamp (default: TRUE)
#' @param show_module Whether to show module name (default: TRUE)
#' @param show_location Whether to show source location (default: FALSE)
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#' # Setup logger with default settings
#' setup_logger()
#'
#' # Setup logger with custom settings
#' setup_logger(io = stdout, level = "Info", show_location = TRUE)
#' }
#' @export
setup_logger <- function(io = stderr,
                         level = "Warn",
                         show_time = TRUE,
                         show_module = TRUE,
                         show_location = FALSE) {
    # Determine the Julia IO object
    if (identical(io, stdout)) {
        jl_io <- julia_eval("stdout")
    } else if (identical(io, stderr)) {
        jl_io <- julia_eval("stderr")
    } else {
        cli::cli_abort("Not implemented: logging into anything other than stdout and stderr")
    }

    # Convert R log level to Julia log level
    if (is.numeric(level)) {
        jl_level <- julia_call("Logging.LogLevel", as.integer(level))
    } else {
        log_levels <- c("Debug", "Info", "Warn", "Error")
        if (!level %in% log_levels) {
            cli::cli_abort("Invalid log level. Must be one of: {.val {log_levels}}, or an integer")
        }
        jl_level <- julia_eval(paste0("Logging.", level))
    }

    # Call the Julia function
    julia_call(
        "TanayLabUtilities.Logger.setup_logger",
        jl_io,
        level = jl_level,
        show_time = show_time,
        show_module = show_module,
        show_location = show_location
    )

    invisible(NULL)
}
