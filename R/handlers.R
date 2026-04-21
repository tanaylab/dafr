.dafr_handlers <- new.env(parent = emptyenv())

#' Register a handler for an action category.
#'
#' Built-in categories: `"inefficient"`. Action is one of `"ignore"`, `"warn"`,
#' `"error"`, or a function `function(message, ...)` invoked by `emit_action()`.
#' @param category Character scalar: the action category.
#' @param action Either a string (`"ignore"`, `"warn"`, `"error"`) or a function
#'   taking the message as its first argument.
#' @return Invisibly `NULL`.
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
