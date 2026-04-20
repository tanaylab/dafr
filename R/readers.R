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

#' Test whether a vector exists on an axis.
#' @param daf A `DafReader`.
#' @param axis Axis name.
#' @param name Vector name.
#' @return Logical scalar.
#' @export
has_vector <- function(daf, axis, name) {
  .assert_name(axis, "axis")
  .assert_name(name, "name")
  format_has_vector(daf, axis, name)
}

#' Names of vectors on an axis, sorted.
#' @inheritParams has_vector
#' @return Character vector.
#' @export
vectors_set <- function(daf, axis) {
  .assert_name(axis, "axis")
  format_vectors_set(daf, axis)
}

#' Get a vector, returning it as an axis-named R vector.
#'
#' @param daf A `DafReader`.
#' @param axis Axis name.
#' @param name Vector name.
#' @param default If supplied and the vector is absent, return a
#'   constant-valued named vector of length `axis_length(daf, axis)`
#'   with the axis entries as names. The vector's atomic type follows
#'   `default` (e.g. `default = NA` yields `logical`, `default = "x"`
#'   yields `character`, `default = 0.0` yields `double`).
#' @return Named atomic vector.
#' @export
get_vector <- function(daf, axis, name, default) {
  .assert_name(axis, "axis")
  .assert_name(name, "name")
  if (!format_has_axis(daf, axis)) {
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
  }
  entries <- format_axis_array(daf, axis)
  if (!format_has_vector(daf, axis, name)) {
    if (missing(default)) {
      stop(sprintf("vector %s does not exist on axis %s",
                   sQuote(name), sQuote(axis)), call. = FALSE)
    }
    out <- rep(default, length(entries))
    names(out) <- entries
    return(out)
  }
  cache_key <- cache_key_vector(axis, name)
  cache_env <- S7::prop(daf, "cache")
  stamp_now <- vector_stamp(daf, axis, name)
  hit <- cache_lookup(cache_env, "memory", cache_key, stamp_now)
  if (!is.null(hit)) return(hit)
  raw <- format_get_vector(daf, axis, name)
  out <- raw
  if (is.null(names(out))) names(out) <- entries
  cache_store(cache_env, "memory", cache_key, out, stamp_now,
              size_bytes = object.size(out))
  out
}

#' Test whether a matrix exists for an axis pair.
#' @param daf A `DafReader`.
#' @param rows_axis Row-axis name.
#' @param cols_axis Column-axis name.
#' @param name Matrix name.
#' @return Logical scalar.
#' @export
has_matrix <- function(daf, rows_axis, cols_axis, name) {
  .assert_name(rows_axis, "rows_axis")
  .assert_name(cols_axis, "cols_axis")
  .assert_name(name,      "name")
  format_has_matrix(daf, rows_axis, cols_axis, name)
}

#' Names of matrices for an axis pair, sorted.
#' @inheritParams has_matrix
#' @return Character vector.
#' @export
matrices_set <- function(daf, rows_axis, cols_axis) {
  .assert_name(rows_axis, "rows_axis")
  .assert_name(cols_axis, "cols_axis")
  format_matrices_set(daf, rows_axis, cols_axis)
}

#' Get a matrix, returning it with axis-entry dimnames.
#'
#' When the matrix is stored only at the flipped-layout axis pair
#' `(cols_axis, rows_axis)`, this function transposes on-the-fly and
#' returns with the requested dimnames.
#'
#' @inheritParams has_matrix
#' @param default If supplied and the matrix is absent under both
#'   layouts, return a constant-valued `nrow x ncol` matrix with axis
#'   entries as dimnames.
#' @return Dense `matrix` or sparse `dgCMatrix` / `lgCMatrix` with
#'   dimnames set.
#' @export
get_matrix <- function(daf, rows_axis, cols_axis, name, default) {
  .assert_name(rows_axis, "rows_axis")
  .assert_name(cols_axis, "cols_axis")
  .assert_name(name,      "name")

  rows <- format_axis_array(daf, rows_axis)
  cols <- format_axis_array(daf, cols_axis)

  primary <- format_has_matrix(daf, rows_axis, cols_axis, name)
  flipped <- !primary && format_has_matrix(daf, cols_axis, rows_axis, name)

  if (!primary && !flipped) {
    if (missing(default)) {
      stop(sprintf("matrix %s does not exist on axes (%s, %s)",
                   sQuote(name), sQuote(rows_axis), sQuote(cols_axis)),
           call. = FALSE)
    }
    out <- matrix(default, nrow = length(rows), ncol = length(cols),
                  dimnames = list(rows, cols))
    return(out)
  }

  if (primary) {
    ra <- rows_axis; ca <- cols_axis
  } else {
    ra <- cols_axis; ca <- rows_axis
  }

  cache_key <- cache_key_matrix(ra, ca, name)
  cache_env <- S7::prop(daf, "cache")
  stamp_now <- matrix_stamp(daf, ra, ca, name)
  stored <- cache_lookup(cache_env, "memory", cache_key, stamp_now)
  if (is.null(stored)) {
    stored <- format_get_matrix(daf, ra, ca, name)
    cache_store(cache_env, "memory", cache_key, stored, stamp_now,
                size_bytes = object.size(stored))
  }

  out <- if (flipped) {
    if (methods::is(stored, "dgCMatrix") || methods::is(stored, "lgCMatrix")) {
      Matrix::t(stored)
    } else {
      t(stored)
    }
  } else {
    stored
  }

  if (methods::is(out, "dgCMatrix") || methods::is(out, "lgCMatrix")) {
    out@Dimnames <- list(rows, cols)
  } else {
    dimnames(out) <- list(rows, cols)
  }
  out
}

#' Human-readable summary of a Daf store.
#'
#' Returns a multi-line string describing axes, scalars, vectors, and
#' matrices. Matches the column-order rendering of Julia DAF's own
#' `description()`.
#'
#' @param daf A `DafReader`.
#' @return Character scalar.
#' @export
description <- function(daf) {
  lines <- c(sprintf("name: %s", S7::prop(daf, "name")),
             sprintf("type: %s", .daf_type_name(daf)))
  sc <- format_scalars_set(daf)
  if (length(sc)) {
    lines <- c(lines, "scalars:")
    for (nm in sc) {
      v <- format_get_scalar(daf, nm)
      lines <- c(lines, sprintf("  %s: %s", nm, .format_scalar_literal(v)))
    }
  }
  axes <- format_axes_set(daf)
  if (length(axes)) {
    lines <- c(lines, "axes:")
    for (ax in axes) {
      lines <- c(lines, sprintf("  %s: %d entries", ax, format_axis_length(daf, ax)))
    }
  }
  vec_axes <- Filter(function(ax) length(format_vectors_set(daf, ax)) > 0L, axes)
  if (length(vec_axes)) {
    lines <- c(lines, "vectors:")
    for (ax in vec_axes) {
      lines <- c(lines, sprintf("  %s:", ax))
      for (nm in format_vectors_set(daf, ax)) {
        lines <- c(lines, sprintf("    %s", nm))
      }
    }
  }
  mat_keys <- character(0L)
  for (ra in axes) for (ca in axes) {
    ms <- format_matrices_set(daf, ra, ca)
    if (length(ms)) mat_keys <- c(mat_keys, sprintf("%s,%s", ra, ca))
  }
  if (length(mat_keys)) {
    lines <- c(lines, "matrices:")
    for (k in mat_keys) {
      parts <- strsplit(k, ",", fixed = TRUE)[[1L]]
      lines <- c(lines, sprintf("  %s:", k))
      for (nm in format_matrices_set(daf, parts[[1L]], parts[[2L]])) {
        lines <- c(lines, sprintf("    %s", nm))
      }
    }
  }
  paste0(paste(lines, collapse = "\n"), "\n")
}

.daf_type_name <- function(daf) {
  cls <- class(daf)[[1L]]
  sub("^dafr::", "", cls)
}

.format_scalar_literal <- function(v) {
  if (is.character(v)) sprintf('"%s"', v)
  else                 format(v)
}
