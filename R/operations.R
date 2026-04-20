#' @include utils.R
NULL

.ops_env <- new.env(parent = emptyenv())
.ops_env$reductions <- list()
.ops_env$eltwise <- list()

#' Register a reduction operation.
#' @param name Op name (character scalar, matches token in query strings).
#' @param fn Function `function(x, ...)` where `x` is a numeric vector /
#'   matrix column and `...` collects named parameters.
#' @return Invisibly `NULL`.
#' @export
register_reduction <- function(name, fn) {
  .assert_name(name, "reduction name")
  stopifnot(is.function(fn))
  .ops_env$reductions[[name]] <- fn
  invisible(NULL)
}

#' Register an eltwise operation.
#' @inheritParams register_reduction
#' @export
register_eltwise <- function(name, fn) {
  .assert_name(name, "eltwise name")
  stopifnot(is.function(fn))
  .ops_env$eltwise[[name]] <- fn
  invisible(NULL)
}

#' @export
get_reduction <- function(name) {
  fn <- .ops_env$reductions[[name]]
  if (is.null(fn)) {
    stop(sprintf("unknown reduction operation: %s", sQuote(name)), call. = FALSE)
  }
  fn
}

#' @export
get_eltwise <- function(name) {
  fn <- .ops_env$eltwise[[name]]
  if (is.null(fn)) {
    stop(sprintf("unknown eltwise operation: %s", sQuote(name)), call. = FALSE)
  }
  fn
}

#' @export
registered_reductions <- function() sort(names(.ops_env$reductions))

#' @export
registered_eltwise <- function() sort(names(.ops_env$eltwise))
