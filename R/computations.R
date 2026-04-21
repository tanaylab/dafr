#' @include classes.R contracts.R
NULL

#' Wrap a function so a contract is enforced on every call.
#'
#' Returns a closure that, on each call, wraps its first (`daf`) argument in
#' a `contractor()`, runs `verify_input()`, executes `fn`, then runs
#' `verify_output()`, and returns `fn`'s result. When contract enforcement is
#' disabled (env `DAF_ENFORCE_CONTRACTS` / option `dafr.enforce_contracts`
#' are both falsy), the wrapping is still performed but verification is a
#' no-op, so the wrapper is cheap to leave in place.
#'
#' Scope: single contract (matching Julia's exported `@computation Contract`
#' form). Two-contract / three-contract variants (Julia UNTESTED) are not
#' supported in this slice; pass a single `Contract` or use a merged one via
#' `merge_contracts`.
#'
#' @param name Human-readable computation name (character scalar). Used in
#'   contract violation messages and as a suffix on the wrapper's daf name.
#' @param contract A `Contract()` describing the inputs and outputs.
#' @param fn The function to wrap. Must accept a `DafReader`/`DafWriter` as
#'   its first argument.
#' @return A function with the same signature as `fn`. The bound `contract`
#'   and `name` are attached as attributes (`dafr_contract`, `dafr_computation_name`).
#' @seealso [function_contract()], [contract_description()], [contractor()].
#' @export
computation <- function(name, contract, fn) {
    .assert_name(name, "name")
    if (!S7::S7_inherits(contract, Contract)) {
        stop("`contract` must be a Contract()", call. = FALSE)
    }
    if (!is.function(fn)) {
        stop("`fn` must be a function", call. = FALSE)
    }

    wrapped <- function(daf, ...) {
        if (!S7::S7_inherits(daf, DafReader)) {
            stop(sprintf(
                "first argument to computation %s must be a DafReader",
                sQuote(name)
            ), call. = FALSE)
        }
        wrapped_daf <- contractor(
            computation = name, contract = contract, daf = daf
        )
        verify_input(wrapped_daf)
        result <- fn(wrapped_daf, ...)
        verify_output(wrapped_daf)
        result
    }
    attr(wrapped, "dafr_contract") <- contract
    attr(wrapped, "dafr_computation_name") <- name
    wrapped
}

#' Retrieve the contract bound to a wrapped computation.
#'
#' Mirror of Julia `function_contract(fn)`. Errors if `fn` was not wrapped
#' by `computation()` in this session.
#'
#' @param fn A function returned by `computation()`.
#' @return A `Contract`.
#' @seealso [computation()].
#' @export
function_contract <- function(fn) {
    contract <- attr(fn, "dafr_contract")
    if (is.null(contract)) {
        stop("no dafr contract bound to this function", call. = FALSE)
    }
    contract
}

#' Render a contract as a human-readable multi-line string.
#'
#' Intended for splicing into roxygen docstrings of functions created by
#' `computation()`. Sections rendered: axes (with expectation + description),
#' scalars, vectors (per axis), matrices (per axis pair).
#'
#' @param contract A `Contract`.
#' @return A character scalar.
#' @seealso [computation()], [Contract()].
#' @export
contract_description <- function(contract) {
    if (!S7::S7_inherits(contract, Contract)) {
        stop("`contract` must be a Contract()", call. = FALSE)
    }
    lines <- character()
    if (length(S7::prop(contract, "axes")) > 0L) {
        lines <- c(lines, "Axes:")
        for (a in names(S7::prop(contract, "axes"))) {
            spec <- S7::prop(contract, "axes")[[a]]
            lines <- c(lines, sprintf("  %s (%s): %s", a, spec[[1L]], spec[[2L]]))
        }
    }
    scalars <- Filter(function(r) identical(r$kind, "scalar"), S7::prop(contract, "data"))
    vectors <- Filter(function(r) identical(r$kind, "vector"), S7::prop(contract, "data"))
    matrices <- Filter(function(r) identical(r$kind, "matrix"), S7::prop(contract, "data"))
    if (length(scalars) > 0L) {
        lines <- c(lines, "Scalars:")
        for (r in scalars) {
            lines <- c(lines, sprintf("  %s (%s, %s): %s",
                r$name, r$expectation, r$type, r$description
            ))
        }
    }
    if (length(vectors) > 0L) {
        lines <- c(lines, "Vectors:")
        for (r in vectors) {
            lines <- c(lines, sprintf("  %s / %s (%s, %s): %s",
                r$axis, r$name, r$expectation, r$type, r$description
            ))
        }
    }
    if (length(matrices) > 0L) {
        lines <- c(lines, "Matrices:")
        for (r in matrices) {
            lines <- c(lines, sprintf("  %s, %s / %s (%s, %s): %s",
                r$rows_axis, r$columns_axis, r$name,
                r$expectation, r$type, r$description
            ))
        }
    }
    paste(lines, collapse = "\n")
}
