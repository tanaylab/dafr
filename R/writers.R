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
  .cli_verbose("add_axis %s (%d entries) on %s",
               axis, length(entries), S7::prop(daf, "name"))
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
  .assert_flag(must_exist, "must_exist")
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
  .assert_flag(overwrite, "overwrite")
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
  .assert_flag(must_exist, "must_exist")
  format_delete_scalar(daf, name, must_exist)
  invisible(daf)
}

#' Set a vector on an axis.
#'
#' @inheritParams has_vector
#' @param vec Atomic vector of length `axis_length(daf, axis)`, or a
#'   named vector whose names are a subset of the axis entries (reordered
#'   into axis order at storage time).
#' @param overwrite See `set_scalar`.
#' @return Invisibly the input `daf`.
#' @export
set_vector <- function(daf, axis, name, vec, overwrite = FALSE) {
  .assert_name(axis, "axis")
  .assert_name(name, "name")
  .assert_flag(overwrite, "overwrite")
  format_set_vector(daf, axis, name, vec, overwrite)
  invisible(daf)
}

#' Delete a vector on an axis.
#'
#' @inheritParams has_vector
#' @param must_exist See `delete_axis`.
#' @return Invisibly the input `daf`.
#' @export
delete_vector <- function(daf, axis, name, must_exist = TRUE) {
  .assert_name(axis, "axis")
  .assert_name(name, "name")
  .assert_flag(must_exist, "must_exist")
  format_delete_vector(daf, axis, name, must_exist)
  invisible(daf)
}

#' Set a matrix indexed by a pair of axes.
#'
#' @inheritParams has_matrix
#' @param mat Dense `matrix`, or sparse `dgCMatrix` / `lgCMatrix`, of
#'   shape `axis_length(rows_axis) x axis_length(cols_axis)`.
#' @param overwrite See `set_scalar`.
#' @return Invisibly the input `daf`.
#' @export
set_matrix <- function(daf, rows_axis, cols_axis, name, mat, overwrite = FALSE) {
  .assert_name(rows_axis, "rows_axis")
  .assert_name(cols_axis, "cols_axis")
  .assert_name(name,      "name")
  .assert_flag(overwrite, "overwrite")
  format_set_matrix(daf, rows_axis, cols_axis, name, mat, overwrite)
  invisible(daf)
}

#' Delete a matrix.
#' @inheritParams has_matrix
#' @param must_exist See `delete_axis`.
#' @return Invisibly the input `daf`.
#' @export
delete_matrix <- function(daf, rows_axis, cols_axis, name, must_exist = TRUE) {
  .assert_name(rows_axis, "rows_axis")
  .assert_name(cols_axis, "cols_axis")
  .assert_name(name,      "name")
  .assert_flag(must_exist, "must_exist")
  format_delete_matrix(daf, rows_axis, cols_axis, name, must_exist)
  invisible(daf)
}

#' Physically store the transposed layout of a matrix.
#'
#' After this call, `get_matrix(cols_axis, rows_axis, name)` skips the
#' transpose-on-the-fly path.
#'
#' @inheritParams has_matrix
#' @return Invisibly the input `daf`.
#' @export
relayout_matrix <- function(daf, rows_axis, cols_axis, name) {
  .assert_name(rows_axis, "rows_axis")
  .assert_name(cols_axis, "cols_axis")
  .assert_name(name,      "name")
  format_relayout_matrix(daf, rows_axis, cols_axis, name)
  invisible(daf)
}
