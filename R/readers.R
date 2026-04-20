#' Test whether an axis exists.
#' @param daf A `DafReader`.
#' @param axis Axis name (character scalar).
#' @return Logical scalar.
#' @export
has_axis <- function(daf, axis) {
  .assert_name(axis, "axis")
  format_has_axis(daf, axis)
}

#' Names of all axes, sorted.
#' @inheritParams has_axis
#' @return Character vector of axis names.
#' @export
axes_set <- function(daf) format_axes_set(daf)

#' Length (entry count) of an axis.
#' @inheritParams has_axis
#' @return Integer scalar.
#' @export
axis_length <- function(daf, axis) {
  .assert_name(axis, "axis")
  format_axis_length(daf, axis)
}

#' Entry-name vector for an axis.
#'
#' @inheritParams has_axis
#' @param null_if_missing If `TRUE`, return `NULL` when the axis is
#'   absent instead of raising.
#' @return Character vector of entry names.
#' @export
axis_vector <- function(daf, axis, null_if_missing = FALSE) {
  .assert_name(axis, "axis")
  if (!format_has_axis(daf, axis)) {
    if (isTRUE(null_if_missing)) return(NULL)
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
  }
  format_axis_array(daf, axis)
}

#' Entry names of an axis (full or by index).
#'
#' @inheritParams has_axis
#' @param indices Optional integer index vector (1-based).
#' @return Character vector.
#' @export
axis_entries <- function(daf, axis, indices = NULL) {
  entries <- axis_vector(daf, axis)
  if (is.null(indices)) return(entries)
  if (!(is.numeric(indices) || is.integer(indices))) {
    stop("`indices` must be an integer vector", call. = FALSE)
  }
  if (anyNA(indices)) {
    stop("`indices` must not contain NA", call. = FALSE)
  }
  if (any(indices != as.integer(indices))) {
    stop("`indices` must be integer-valued", call. = FALSE)
  }
  indices <- as.integer(indices)
  if (any(indices < 1L | indices > length(entries))) {
    stop(sprintf("indices out of range [1, %d]", length(entries)), call. = FALSE)
  }
  entries[indices]
}

#' Look up 1-based positions of entries in an axis.
#'
#' @inheritParams has_axis
#' @param entries Character vector of entry names to resolve.
#' @return Integer vector of 1-based positions; same length as `entries`.
#' @export
axis_indices <- function(daf, axis, entries) {
  .assert_name(axis, "axis")
  if (!is.character(entries)) stop("`entries` must be a character vector", call. = FALSE)
  if (anyNA(entries))         stop("`entries` must not contain NA",        call. = FALSE)
  dict <- format_axis_dict(daf, axis)
  out <- vapply(entries, function(nm) {
    v <- dict[[nm]]
    if (is.null(v)) NA_integer_ else as.integer(v)
  }, integer(1L), USE.NAMES = FALSE)
  missing <- is.na(out)
  if (any(missing)) {
    stop(sprintf("entries not found in axis %s: %s",
                 sQuote(axis),
                 paste(sQuote(entries[missing]), collapse = ", ")),
         call. = FALSE)
  }
  out
}

#' Entry-name to 1-based-index hash for an axis.
#' @inheritParams has_axis
#' @return An environment mapping entry names to integer positions.
#' @export
axis_dict <- function(daf, axis) {
  .assert_name(axis, "axis")
  format_axis_dict(daf, axis)
}

#' Test whether a scalar exists.
#' @param daf A `DafReader`.
#' @param name Scalar name.
#' @return Logical scalar.
#' @export
has_scalar <- function(daf, name) {
  .assert_name(name, "name")
  format_has_scalar(daf, name)
}

#' Names of all scalars, sorted.
#' @inheritParams has_scalar
#' @return Character vector.
#' @export
scalars_set <- function(daf) format_scalars_set(daf)

#' Get a scalar, optionally with a default when missing.
#' @inheritParams has_scalar
#' @param default Value to return when the scalar is absent. If missing
#'   and the scalar is absent, an error is raised.
#' @return The scalar value.
#' @export
get_scalar <- function(daf, name, default) {
  .assert_name(name, "name")
  if (format_has_scalar(daf, name)) {
    return(format_get_scalar(daf, name))
  }
  if (!missing(default)) return(default)
  stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
}
