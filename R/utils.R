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

# Helper function to handle pipe logic for functions with ONE required argument followed by ...
# Takes the value passed to the required argument, whether it was explicitly missing, and the dots list.
# Returns a list containing:
#   $query: The identified JuliaObject query, or NULL.
#   $value: The actual value intended for the required argument.
#   $provided: Logical indicating if the actual value was successfully provided.
handle_single_required_arg_pipe <- function(required_arg_val, # The actual argument value passed
                                            required_arg_missing, # Result of missing(arg_name) in caller
                                            dots) { # Result of list(...) in caller
    query <- NULL
    actual_value <- required_arg_val
    value_provided <- !required_arg_missing

    if (value_provided && inherits(required_arg_val, "JuliaObject")) {
        # The required argument slot was filled by the query object
        query <- required_arg_val
        if (length(dots) > 0 && !inherits(dots[[1]], "JuliaObject")) {
            # The actual value is the first element in dots
            actual_value <- dots[[1]]
            # Value is now considered provided via dots
        } else {
            # Query was passed as main arg, but no actual value found in dots
            value_provided <- FALSE
            actual_value <- NULL # Ensure value is NULL if not provided
        }
    } else {
        # The required argument slot has the actual value (or was missing)
        # Check if the query object is in dots
        if (length(dots) > 0 && inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
            # If query is in dots, the actual value (if provided) is already in actual_value
        }
        # If the argument was missing initially (!value_provided), it remains not provided.
        # If the argument was provided and not a query, it remains provided.
    }
    # Ensure value is NULL if it wasn't provided, even if required_arg_val wasn't NULL initially (e.g. default)
    if (!value_provided) {
        actual_value <- NULL
    }
    return(list(query = query, value = actual_value, provided = value_provided))
}

# Helper function that combines pipe handling and common validation
# Validates that the required argument was provided and matches the expected type.
# arg_name is used for error messages.
# type_check_fun is a function like is.character, is.numeric etc.
# type_error_msg is the message for the type validation failure.
handle_query_pipe_and_validate <- function(arg_val,
                                           arg_missing,
                                           dots,
                                           arg_name,
                                           type_check_fun = is.character,
                                           type_error_msg = paste0("{.field ", arg_name, "} must be a character string")) {
    res <- handle_single_required_arg_pipe(arg_val, arg_missing, dots)

    if (!res$provided) {
        cli::cli_abort("argument {.field {arg_name}} is missing with no default", call = parent.frame())
    }
    if (!type_check_fun(res$value)) {
        cli::cli_abort(type_error_msg, call = parent.frame())
    }
    return(res)
}

# Helper function for parameterless operations that can be piped.
# Handles extracting the query from dots, calling the Julia function,
# and applying the pipe operator.
handle_parameterless_operation_pipe <- function(dots, julia_function_name, op_name) {
    query <- NULL
    # Operation functions expect the query object directly in dots
    if (length(dots) == 1 && inherits(dots[[1]], "JuliaObject")) {
        query <- dots[[1]]
    } else if (length(dots) > 0) {
        # Use the provided operation name for the error message
        cli::cli_abort("{.code {op_name}} expects zero arguments when not used with pipe, or one query argument when used with pipe.", call = parent.frame())
    }

    result <- julia_call(julia_function_name)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }
    return(result)
}

# Helper function for operations/queries with ONE optional argument + pipe.
# Handles extracting the query and the actual argument value.
# Returns a list: $query, $value
handle_optional_arg_pipe <- function(arg_val, arg_missing, dots) {
    query <- NULL
    actual_value <- arg_val # Takes default if arg_missing

    # Handle pipe logic for optional argument
    arg_is_query <- !arg_missing && inherits(arg_val, "JuliaObject")
    dots_has_query <- length(dots) > 0 && inherits(dots[[1]], "JuliaObject")

    if (arg_is_query) {
        query <- arg_val
        # If arg was query, the actual value might be in dots, otherwise it's NULL (or its default)
        if (length(dots) > 0 && !inherits(dots[[1]], "JuliaObject")) {
            actual_value <- dots[[1]]
        } else {
            actual_value <- NULL # Arg was query, no replacement value in dots. Reset to NULL default.
            # Assumes the default for these functions is NULL.
        }
    } else {
        # Arg is not the query (or was missing/default), use its value
        # actual_value already holds the correct value (passed arg or default)
        if (dots_has_query) {
            query <- dots[[1]]
        }
    }
    return(list(query = query, value = actual_value))
}
