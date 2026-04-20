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
