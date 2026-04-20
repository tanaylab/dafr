#' @include query_eval.R query_parse.R
NULL

# Public entry points: parse_query, get_query, has_query,
# is_axis_query, query_axis_name, query_result_dimensions,
# get_frame, q().

#' Evaluate a query string against a daf reader.
#' @param daf A `DafReader`.
#' @param query_string A query string.
#' @return A scalar, vector, matrix, names set, or NULL if missing.
#' @export
get_query <- function(daf, query_string) {
  ast <- parse_query(query_string)
  .eval_query(daf, ast)
}

#' Canonicalise a query string.
#' @inheritParams get_query
#' @return The canonical query string (stable form; suitable for use as cache key).
#' @export
canonical_query <- function(query_string) {
  .canonicalise_ast(parse_query(query_string))
}

#' Extract a data.frame of vectors along one axis.
#' @param daf A DafReader.
#' @param axis_query A query string that evaluates to an axis entry vector
#'   (optionally filtered via mask).
#' @param columns Optional character vector of vector names. Default: all
#'   vectors for the axis.
#' @return A data.frame with one column per vector, rows named by axis entries.
#' @export
get_frame <- function(daf, axis_query, columns = NULL) {
  axis_ast <- parse_query(axis_query)
  state <- list(kind = "init", value = NULL, if_missing = NULL)
  for (node in axis_ast) state <- .apply_node(node, state, daf)
  if (!identical(state$kind, "axis")) {
    stop("axis_query did not resolve to an axis", call. = FALSE)
  }
  entries <- state$value
  axis_name <- state$axis
  if (is.null(columns)) columns <- format_vectors_set(daf, axis_name)
  cols <- lapply(columns, function(nm) {
    v <- format_get_vector(daf, axis_name, nm)
    full_entries <- format_axis_array(daf, axis_name)
    idx <- match(entries, full_entries)
    v[idx]
  })
  names(cols) <- columns
  as.data.frame(cols, row.names = entries,
                stringsAsFactors = FALSE, optional = TRUE)
}
