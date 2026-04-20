#' @include classes.R queries.R format_api.R
NULL

# Sentinel constants for wildcard view specifications.
#' Wildcard for all axes.
#' @export
ALL_AXES     <- "*"
#' Wildcard for all scalars.
#' @export
ALL_SCALARS  <- "*"
#' Wildcard for all vectors.
#' @export
ALL_VECTORS  <- c("*", "*")
#' Wildcard for all matrices.
#' @export
ALL_MATRICES <- c("*", "*", "*")

# Default view specs (composed below).
#' @export
VIEW_ALL_AXES     <- list(ALL_AXES     = "=")
#' @export
VIEW_ALL_SCALARS  <- list(ALL_SCALARS  = "=")
#' @export
VIEW_ALL_VECTORS  <- list(ALL_VECTORS  = "=")
#' @export
VIEW_ALL_MATRICES <- list(ALL_MATRICES = "=")
#' @export
VIEW_ALL_DATA     <- list(VIEW_ALL_SCALARS, VIEW_ALL_VECTORS, VIEW_ALL_MATRICES)

#' A read-only view over a base daf.
#'
#' A ViewDaf carries a reference to a base `DafReader` and a dictionary of
#' per-name query overrides (axes / scalars / vectors / matrices). Reads on
#' the view rewrite into queries against the base; no data is copied.
#' @export
ViewDaf <- S7::new_class(
  name = "ViewDaf",
  package = "dafr",
  parent = DafReadOnly,
  properties = list(
    base          = DafReader,
    view_axes     = S7::class_list,
    view_scalars  = S7::class_list,
    view_vectors  = S7::class_list,
    view_matrices = S7::class_list
  )
)

#' Construct a ViewDaf over a base daf.
#' @param daf Base `DafReader`.
#' @param name Name for the view (defaults to `<daf-name>.view`).
#' @param axes Optional list of axis overrides. V1 path: `NULL` means
#'   expose every base axis as-is.
#' @param data Optional list of data overrides. V1 path: unused; the
#'   no-override behaviour is always "expose everything as-is".
#' @return A `ViewDaf`.
#' @export
viewer <- function(daf, name = NULL, axes = NULL, data = NULL) {
  if (is.null(name)) name <- paste0(S7::prop(daf, "name"), ".view")
  ViewDaf(
    name                    = name,
    internal                = new_internal_env(),
    cache                   = new_cache_env(),
    axis_version_counter    = new_counter_env(),
    vector_version_counter  = new_counter_env(),
    matrix_version_counter  = new_counter_env(),
    base                    = daf,
    view_axes               = .resolve_view_axes(daf, axes),
    view_scalars            = .resolve_view_scalars(daf, data),
    view_vectors            = .resolve_view_vectors(daf, data),
    view_matrices           = .resolve_view_matrices(daf, data)
  )
}

.resolve_view_axes <- function(daf, axes) {
  if (is.null(axes)) {
    return(setNames(rep(list("="), length(format_axes_set(daf))),
                     format_axes_set(daf)))
  }
  stop("view-axes override not yet implemented (see Task V3)", call. = FALSE)
}

.resolve_view_scalars <- function(daf, data) {
  setNames(as.list(format_scalars_set(daf)), format_scalars_set(daf))
}

.resolve_view_vectors <- function(daf, data) {
  out <- list()
  for (a in format_axes_set(daf)) {
    for (v in format_vectors_set(daf, a)) {
      out[[paste(a, v, sep = "|")]] <- list(axis = a, name = v, query = "=")
    }
  }
  out
}

.resolve_view_matrices <- function(daf, data) {
  out <- list()
  for (r in format_axes_set(daf)) {
    for (c in format_axes_set(daf)) {
      for (m in format_matrices_set(daf, r, c)) {
        out[[paste(r, c, m, sep = "|")]] <- list(rows = r, cols = c,
                                                   name = m, query = "=")
      }
    }
  }
  out
}

# --- Query rewriters ----------------------------------------------------

.view_query_for_scalar <- function(view, name) {
  override <- view@view_scalars[[name]]
  if (is.null(override) || identical(override, "=") || identical(override, name)) {
    return(paste0(". ", name))
  }
  override
}

.view_query_for_axis <- function(view, axis) {
  override <- view@view_axes[[axis]]
  if (is.null(override) || identical(override, "=") || identical(override, axis)) {
    return(paste0("@ ", axis))
  }
  override
}

.view_query_for_vector <- function(view, axis, name) {
  key <- paste(axis, name, sep = "|")
  override <- view@view_vectors[[key]]
  if (is.null(override) || identical(override$query, "=")) {
    return(sprintf("@ %s : %s", axis, name))
  }
  override$query
}

.view_query_for_matrix <- function(view, rows_axis, columns_axis, name) {
  key <- paste(rows_axis, columns_axis, name, sep = "|")
  override <- view@view_matrices[[key]]
  if (is.null(override) || identical(override$query, "=")) {
    return(sprintf("@ %s @ %s :: %s", rows_axis, columns_axis, name))
  }
  override$query
}

# --- format_* dispatch --------------------------------------------------

S7::method(format_has_scalar,
           list(ViewDaf, S7::class_character)) <- function(daf, name) {
  if (!(name %in% names(daf@view_scalars))) return(FALSE)
  q_str <- .view_query_for_scalar(daf, name)
  has_query(daf@base, q_str)
}

S7::method(format_get_scalar,
           list(ViewDaf, S7::class_character)) <- function(daf, name) {
  get_query(daf@base, .view_query_for_scalar(daf, name))
}

S7::method(format_scalars_set, ViewDaf) <- function(daf) {
  sort(names(daf@view_scalars), method = "radix")
}

S7::method(format_has_axis,
           list(ViewDaf, S7::class_character)) <- function(daf, axis) {
  !is.null(daf@view_axes[[axis]])
}

S7::method(format_axes_set, ViewDaf) <- function(daf) {
  sort(names(daf@view_axes), method = "radix")
}

S7::method(format_axis_length,
           list(ViewDaf, S7::class_character)) <- function(daf, axis) {
  length(format_axis_array(daf, axis))
}

S7::method(format_axis_array,
           list(ViewDaf, S7::class_character)) <- function(daf, axis) {
  get_query(daf@base, .view_query_for_axis(daf, axis))
}

S7::method(format_has_vector,
           list(ViewDaf, S7::class_character, S7::class_character)) <- function(daf, axis, name) {
  key <- paste(axis, name, sep = "|")
  !is.null(daf@view_vectors[[key]])
}

S7::method(format_vectors_set,
           list(ViewDaf, S7::class_character)) <- function(daf, axis) {
  keys <- names(daf@view_vectors)
  prefix <- paste0(axis, "|")
  sub(prefix, "", keys[startsWith(keys, prefix)], fixed = TRUE)
}

S7::method(format_get_vector,
           list(ViewDaf, S7::class_character, S7::class_character)) <- function(daf, axis, name) {
  get_query(daf@base, .view_query_for_vector(daf, axis, name))
}

S7::method(format_has_matrix,
           list(ViewDaf, S7::class_character, S7::class_character, S7::class_character)) <- function(daf, rows_axis, columns_axis, name) {
  key <- paste(rows_axis, columns_axis, name, sep = "|")
  !is.null(daf@view_matrices[[key]])
}

S7::method(format_matrices_set,
           list(ViewDaf, S7::class_character, S7::class_character)) <- function(daf, rows_axis, columns_axis) {
  keys <- names(daf@view_matrices)
  prefix <- paste(rows_axis, columns_axis, "", sep = "|")
  sub(prefix, "", keys[startsWith(keys, prefix)], fixed = TRUE)
}

S7::method(format_get_matrix,
           list(ViewDaf, S7::class_character, S7::class_character, S7::class_character)) <- function(daf, rows_axis, columns_axis, name) {
  get_query(daf@base, .view_query_for_matrix(daf, rows_axis, columns_axis, name))
}
