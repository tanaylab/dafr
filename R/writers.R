#' Add a new axis.
#'
#' @param daf A `DafWriter`.
#' @param axis Axis name.
#' @param entries Unique, non-NA, non-empty character vector of entry names.
#' @return Invisibly the input `daf`.
#' @export
add_axis <- function(daf, axis, entries) {
  .assert_name(axis, "axis")
  stopifnot(is.character(entries))
  format_add_axis(daf, axis, entries)
  invisible(daf)
}

#' Delete an axis (and all vectors / matrices that depend on it).
#'
#' @inheritParams add_axis
#' @param must_exist If `TRUE` (default) raise when the axis is absent;
#'   if `FALSE` silently no-op.
#' @return Invisibly the input `daf`.
#' @export
delete_axis <- function(daf, axis, must_exist = TRUE) {
  .assert_name(axis, "axis")
  stopifnot(is.logical(must_exist), length(must_exist) == 1L, !is.na(must_exist))
  format_delete_axis(daf, axis, must_exist)
  invisible(daf)
}

#' Set a scalar.
#' @inheritParams has_scalar
#' @param value Atomic scalar (length 1, non-NA).
#' @param overwrite If `FALSE` (default) error when the scalar already
#'   exists; if `TRUE` replace.
#' @return Invisibly the input `daf`.
#' @export
set_scalar <- function(daf, name, value, overwrite = FALSE) {
  .assert_name(name, "name")
  stopifnot(is.logical(overwrite), length(overwrite) == 1L, !is.na(overwrite))
  format_set_scalar(daf, name, value, overwrite)
  invisible(daf)
}

#' Delete a scalar.
#' @inheritParams has_scalar
#' @param must_exist See `delete_axis`.
#' @return Invisibly the input `daf`.
#' @export
delete_scalar <- function(daf, name, must_exist = TRUE) {
  .assert_name(name, "name")
  stopifnot(is.logical(must_exist), length(must_exist) == 1L, !is.na(must_exist))
  format_delete_scalar(daf, name, must_exist)
  invisible(daf)
}
