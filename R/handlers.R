.dafr_handlers <- new.env(parent = emptyenv())

#' Register a handler for an action category.
#'
#' Built-in categories: `"inefficient"`. Action is one of `"ignore"`, `"warn"`,
#' `"error"`, or a function `function(message, ...)` invoked by `emit_action()`.
#' @param category Character scalar: the action category.
#' @param action Either a string (`"ignore"`, `"warn"`, `"error"`) or a function
#'   taking the message as its first argument.
#' @return Invisibly `NULL`.
#' @examples
#' register_dafr_handler("inefficient", "ignore")
#' register_dafr_handler("inefficient", "warn")
#' @export
register_dafr_handler <- function(category, action) {
    stopifnot(is.character(category), length(category) == 1L)
    if (!(is.character(action) || is.function(action))) {
        stop("action must be a string or a function")
    }
    if (is.character(action) && !action %in% c("ignore", "warn", "error")) {
        stop('action must be one of "ignore", "warn", "error", or a function')
    }
    assign(category, action, envir = .dafr_handlers)
    invisible()
}

#' Emit an action in a category; dispatches per registered handler or option.
#' @noRd
emit_action <- function(category, message) {
    handler <- if (exists(category, envir = .dafr_handlers, inherits = FALSE)) {
        get(category, envir = .dafr_handlers)
    } else {
        dafr_opt(paste0("dafr.", category))
    }
    if (is.function(handler)) {
        handler(message)
    } else {
        switch(handler,
            ignore = invisible(NULL),
            warn   = warning(message, call. = FALSE),
            error  = stop(message, call. = FALSE),
            stop("unknown handler action: ", handler)
        )
    }
}

#' Inefficient-action handler constants.
#'
#' String constants matching the lowercase tokens accepted by
#' [register_dafr_handler()]. Pass these to
#' [inefficient_action_handler()] or any registry entry that takes
#' an action token.
#'
#' @return Character scalar (one of `"error"`, `"warn"`, `"ignore"`).
#' @examples
#' inefficient_action_handler(WARN_HANDLER)
#' @name handler-constants
NULL

#' @rdname handler-constants
#' @export
ERROR_HANDLER <- "error"

#' @rdname handler-constants
#' @export
WARN_HANDLER <- "warn"

#' @rdname handler-constants
#' @export
IGNORE_HANDLER <- "ignore"

#' Register a handler for the `"inefficient"` action category.
#'
#' Thin wrapper around `register_dafr_handler("inefficient", handler)`.
#' Exists to match the Julia-facade wrapper's API.
#'
#' @param handler One of [`ERROR_HANDLER`][handler-constants],
#'   [`WARN_HANDLER`][handler-constants],
#'   [`IGNORE_HANDLER`][handler-constants], or a function
#'   `function(message, ...)`.
#' @return Invisibly `NULL`.
#' @examples
#' inefficient_action_handler(IGNORE_HANDLER)
#' inefficient_action_handler(function(msg) message("inefficient: ", msg))
#' @export
inefficient_action_handler <- function(handler) {
    register_dafr_handler("inefficient", handler)
    invisible()
}
