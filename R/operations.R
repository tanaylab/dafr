#' @include utils.R
NULL

.ops_env <- new.env(parent = emptyenv())
.ops_env$reductions <- list()
.ops_env$eltwise <- list()

#' Register a reduction operation.
#'
#' Stores a user-defined reduction function under the given name so that it
#' can be retrieved by [get_reduction()] and invoked during query evaluation.
#'
#' @param name Op name (character scalar, matches token in query strings).
#' @param fn Function `function(x, ...)` where `x` is a numeric vector or
#'   matrix column and `...` collects named parameters.
#' @param overwrite Logical scalar; set to `TRUE` to replace an already-
#'   registered operation.
#' @return Invisibly `NULL`.
#' @examples
#' register_reduction("Median_example", function(x, ...) median(x, ...), overwrite = TRUE)
#' registered_reductions()
#' @export
register_reduction <- function(name, fn, overwrite = FALSE) {
    .assert_name(name, "reduction name")
    if (!is.function(fn)) {
        stop(sprintf("`fn` must be a function (for reduction %s)", sQuote(name)),
            call. = FALSE
        )
    }
    if (!isTRUE(overwrite) && !is.null(.ops_env$reductions[[name]])) {
        stop(sprintf(
            "reduction operation %s already registered; use overwrite = TRUE",
            sQuote(name)
        ), call. = FALSE)
    }
    .ops_env$reductions[[name]] <- fn
    invisible(NULL)
}

#' Register an eltwise operation.
#'
#' Stores a user-defined element-wise function under the given name so that it
#' can be retrieved by [get_eltwise()] and invoked during query evaluation.
#'
#' @param name Op name (character scalar, matches token in query strings).
#' @param fn Function `function(x, ...)` where `x` is a numeric vector or
#'   matrix (eltwise ops preserve shape) and `...` collects named parameters.
#' @param overwrite Logical scalar; set to `TRUE` to replace an already-
#'   registered operation.
#' @return Invisibly `NULL`.
#' @examples
#' register_eltwise("Clamp01", function(x, ...) pmin(pmax(x, 0), 1),
#'                  overwrite = TRUE)
#' registered_eltwise()
#' @export
register_eltwise <- function(name, fn, overwrite = FALSE) {
    .assert_name(name, "eltwise name")
    if (!is.function(fn)) {
        stop(sprintf("`fn` must be a function (for eltwise %s)", sQuote(name)),
            call. = FALSE
        )
    }
    if (!isTRUE(overwrite) && !is.null(.ops_env$eltwise[[name]])) {
        stop(sprintf(
            "eltwise operation %s already registered; use overwrite = TRUE",
            sQuote(name)
        ), call. = FALSE)
    }
    .ops_env$eltwise[[name]] <- fn
    invisible(NULL)
}

#' Retrieve a registered reduction operation by name.
#'
#' Looks up a reduction operation previously stored via
#' [register_reduction()]. Raises if the name is not registered.
#'
#' @param name Op name (character scalar).
#' @return The stored function.
#' @examples
#' registered_reductions()   # "Sum" is among the built-ins
#' fn <- get_reduction("Sum")
#' fn(c(1, 2, 3))
#' @export
get_reduction <- function(name) {
    fn <- .ops_env$reductions[[name]]
    if (is.null(fn)) {
        stop(sprintf("unknown reduction operation: %s", sQuote(name)), call. = FALSE)
    }
    fn
}

#' Retrieve a registered eltwise operation by name.
#'
#' Looks up an eltwise operation previously stored via
#' [register_eltwise()]. Raises if the name is not registered.
#'
#' @param name Op name (character scalar).
#' @return The stored function.
#' @examples
#' registered_eltwise()      # "Abs" is among the built-ins
#' fn <- get_eltwise("Abs")
#' fn(c(-1, 2, -3))
#' @export
get_eltwise <- function(name) {
    fn <- .ops_env$eltwise[[name]]
    if (is.null(fn)) {
        stop(sprintf("unknown eltwise operation: %s", sQuote(name)), call. = FALSE)
    }
    fn
}

#' List registered reduction operations.
#'
#' Returns the names of all currently registered reduction operations.
#'
#' @return Sorted character vector of registered reduction names.
#' @examples
#' head(registered_reductions())
#' register_reduction("Median_example", function(x, ...) median(x), overwrite = TRUE)
#' "Median_example" %in% registered_reductions()
#' @export
registered_reductions <- function() sort(names(.ops_env$reductions))

#' List registered eltwise operations.
#'
#' Returns the names of all currently registered eltwise operations.
#'
#' @return Sorted character vector of registered eltwise names.
#' @examples
#' head(registered_eltwise())
#' register_eltwise("Negate_example", function(x, ...) -x, overwrite = TRUE)
#' "Negate_example" %in% registered_eltwise()
#' @export
registered_eltwise <- function() sort(names(.ops_env$eltwise))

.op_sum <- function(x, ..., na_rm = FALSE) sum(x, na.rm = na_rm)
.op_mean <- function(x, ..., na_rm = FALSE) mean(x, na.rm = na_rm)
.op_max <- function(x, ..., na_rm = FALSE) max(x, na.rm = na_rm)
.op_min <- function(x, ..., na_rm = FALSE) min(x, na.rm = na_rm)
.op_count <- function(x, ...) length(x)

.op_log <- function(x, ..., eps = 0, base = exp(1)) log(x + eps, base = base)
.op_abs <- function(x, ...) abs(x)
.op_exp <- function(x, ...) exp(x)
.op_sqrt <- function(x, ...) sqrt(x)
.op_round <- function(x, ..., digits = 0) round(x, digits = digits)

.op_clamp <- function(x, ..., min = -Inf, max = Inf) {
    if (min >= max) {
        stop(sprintf("Clamp: min (%g) must be strictly less than max (%g)", min, max),
            call. = FALSE
        )
    }
    if (methods::is(x, "dgCMatrix")) {
        if (min <= 0 && 0 <= max) {
            out <- x
            out@x <- pmin(pmax(out@x, min), max)
            return(out)
        }
        x <- as.matrix(x)
    }
    pmin(pmax(x, min), max)
}

.op_convert <- function(x, ..., type) {
    if (missing(type)) {
        stop("Convert: 'type' parameter is required (one of 'double', 'integer', 'logical')",
            call. = FALSE
        )
    }
    if (!is.character(type) || length(type) != 1L || !type %in% c("double", "integer", "logical")) {
        stop(sprintf(
            "Convert: 'type' must be one of 'double', 'integer', 'logical' (got %s)",
            sQuote(as.character(type)[1L])
        ), call. = FALSE)
    }
    if (methods::is(x, "dgCMatrix") && type == "double") {
        return(x)
    }
    if (methods::is(x, "dgCMatrix")) {
        x <- as.matrix(x)
    }
    storage.mode(x) <- type
    x
}

attr(.op_sum, ".dafr_builtin") <- "Sum"
attr(.op_mean, ".dafr_builtin") <- "Mean"
attr(.op_max, ".dafr_builtin") <- "Max"
attr(.op_min, ".dafr_builtin") <- "Min"
attr(.op_count, ".dafr_builtin") <- "Count"
attr(.op_log, ".dafr_builtin") <- "Log"
attr(.op_abs, ".dafr_builtin") <- "Abs"
attr(.op_exp, ".dafr_builtin") <- "Exp"
attr(.op_sqrt, ".dafr_builtin") <- "Sqrt"
attr(.op_round, ".dafr_builtin") <- "Round"
attr(.op_clamp, ".dafr_builtin") <- "Clamp"
attr(.op_convert, ".dafr_builtin") <- "Convert"

.register_default_ops <- function() {
    register_reduction("Sum", .op_sum, overwrite = TRUE)
    register_reduction("Mean", .op_mean, overwrite = TRUE)
    register_reduction("Max", .op_max, overwrite = TRUE)
    register_reduction("Min", .op_min, overwrite = TRUE)
    register_reduction("Count", .op_count, overwrite = TRUE)

    register_eltwise("Log", .op_log, overwrite = TRUE)
    register_eltwise("Abs", .op_abs, overwrite = TRUE)
    register_eltwise("Exp", .op_exp, overwrite = TRUE)
    register_eltwise("Sqrt", .op_sqrt, overwrite = TRUE)
    register_eltwise("Round", .op_round, overwrite = TRUE)
    register_eltwise("Clamp", .op_clamp, overwrite = TRUE)
    register_eltwise("Convert", .op_convert, overwrite = TRUE)

    invisible(NULL)
}
