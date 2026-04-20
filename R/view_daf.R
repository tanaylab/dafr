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
