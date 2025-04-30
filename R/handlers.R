#' Handler types for abnormal operations
#'
#' These constants define the different types of handlers for abnormal (but recoverable) operations.
#'
#' @name abnormal_handlers
#' @export
IGNORE_HANDLER <- "IgnoreHandler"

#' @rdname abnormal_handlers
#' @export
WARN_HANDLER <- "WarnHandler"

#' @rdname abnormal_handlers
#' @export
ERROR_HANDLER <- "ErrorHandler"

#' Get Julia handler object from handler name
#'
#' Internal utility function to convert R handler names to Julia handler objects.
#'
#' @param handler One of "IgnoreHandler", "WarnHandler", or "ErrorHandler"
#' @return A Julia handler object
#' @noRd
get_julia_handler <- function(handler) {
    valid_handlers <- c(IGNORE_HANDLER, WARN_HANDLER, ERROR_HANDLER)
    if (!handler %in% valid_handlers) {
        cli::cli_abort("Handler must be one of: {.val {valid_handlers}}")
    }
    julia_eval(paste0("TanayLabUtilities.", handler))
}

#' Set the handler for inefficient matrix access
#'
#' Specify the handler to use when accessing a matrix in an inefficient way ("against the grain").
#' Returns the previous handler as a string.
#'
#' @param handler The handler to use, one of "IgnoreHandler", "WarnHandler", or "ErrorHandler"
#' @return The previous handler name
#' @export
inefficient_action_handler <- function(handler) {
    jl_handler <- get_julia_handler(handler)

    # Call the Julia function to set the handler and get the previous one
    previous_handler <- julia_call("_inefficient_action_handler", jl_handler)

    # Convert the returned Julia handler object back to an R string
    if (julia_call("Base.isequal", previous_handler, julia_eval("TanayLabUtilities.IgnoreHandler"))) {
        return(IGNORE_HANDLER)
    } else if (julia_call("Base.isequal", previous_handler, julia_eval("TanayLabUtilities.WarnHandler"))) {
        return(WARN_HANDLER)
    } else if (julia_call("Base.isequal", previous_handler, julia_eval("TanayLabUtilities.ErrorHandler"))) {
        return(ERROR_HANDLER)
    } else {
        cli::cli_abort("Unknown handler type returned from Julia")
    }
}
