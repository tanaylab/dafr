#' @include query_eval.R query_parse.R
NULL

# Public entry points: parse_query, get_query, has_query,
# is_axis_query, query_axis_name, query_result_dimensions,
# get_frame.

#' Evaluate a query string against a daf reader.
#' @param daf A `DafReader`.
#' @param query_string A query string.
#' @return A scalar, vector, matrix, names set, or NULL if missing.
#' @export
get_query <- function(daf, query_string) {
  ast    <- parse_query(query_string)
  canon  <- .canonicalise_ast(ast)
  key    <- cache_key_query(canon)
  touched <- .collect_query_versions(daf, ast)
  stamp   <- .snapshot_versions(daf, touched)
  cache_env <- S7::prop(daf, "cache")
  cached <- cache_lookup(cache_env, "query", key, stamp)
  if (!is.null(cached)) return(cached)
  value <- .eval_query(daf, ast)
  cache_store(cache_env, "query", key, value, stamp,
              size_bytes = as.numeric(object.size(value)))
  value
}

# Scan AST nodes and collect which axes/vectors/matrices the query reads.
# Returns list(axes, vectors, matrices) where vectors is a named list
# axis -> char vector of names, matrices is a named list "rows:cols" -> names.
.collect_query_versions <- function(daf, ast) {
  axes       <- character(0)
  vecs       <- list()    # axis -> char vec of vector names
  mats       <- list()    # "rows:cols" -> char vec of matrix names
  scope_axis <- NULL
  rows_axis  <- NULL
  cols_axis  <- NULL
  two_axes   <- FALSE

  for (n in ast) {
    switch(n$op,
      Axis = {
        axes <- c(axes, n$axis_name)
        if (isTRUE(two_axes)) {
          # already in two-axis scope; ignore extra Axis nodes
        } else if (!is.null(scope_axis)) {
          rows_axis <- scope_axis
          cols_axis <- n$axis_name
          two_axes  <- TRUE
        } else {
          scope_axis <- n$axis_name
        }
      },
      LookupVector = {
        if (!is.null(n$name) && !is.null(scope_axis) && !isTRUE(two_axes)) {
          vecs[[scope_axis]] <- c(vecs[[scope_axis]], n$name)
        }
      },
      LookupMatrix = {
        if (!is.null(n$name) && isTRUE(two_axes)) {
          mk <- paste0(rows_axis, ":", cols_axis)
          mats[[mk]] <- c(mats[[mk]], n$name)
        }
      },
      BeginMask = , BeginNegatedMask = ,
      AndMask = , AndNegatedMask = ,
      OrMask = , OrNegatedMask = ,
      XorMask = , XorNegatedMask = {
        if (!is.null(n$property) && !is.null(scope_axis)) {
          vecs[[scope_axis]] <- c(vecs[[scope_axis]], n$property)
        }
      },
      GroupBy = , GroupRowsBy = , GroupColumnsBy = , CountBy = {
        if (!is.null(n$property)) {
          ax <- if (isTRUE(two_axes)) {
            if (identical(n$op, "GroupRowsBy")) rows_axis
            else if (identical(n$op, "GroupColumnsBy")) cols_axis
            else scope_axis
          } else scope_axis
          if (!is.null(ax)) vecs[[ax]] <- c(vecs[[ax]], n$property)
        }
      },
      NULL  # ignore all other nodes
    )
  }
  list(
    axes     = unique(axes),
    vectors  = lapply(vecs, unique),
    matrices = lapply(mats, unique)
  )
}

# Build a snapshot of the current version counters for the touched data.
# Returns a named list suitable for use as a cache stamp.
.snapshot_versions <- function(daf, touched) {
  ax_ctr  <- S7::prop(daf, "axis_version_counter")
  vec_ctr <- S7::prop(daf, "vector_version_counter")
  mat_ctr <- S7::prop(daf, "matrix_version_counter")

  axes_snap <- vapply(touched$axes, function(a) {
    v <- ax_ctr[[a]]
    if (is.null(v)) 0L else v
  }, integer(1))

  vecs_snap <- unlist(lapply(names(touched$vectors), function(a) {
    vapply(touched$vectors[[a]], function(nm) {
      k <- paste0(a, ":", nm)
      x <- vec_ctr[[k]]
      if (is.null(x)) 0L else x
    }, integer(1))
  }), use.names = TRUE)

  mats_snap <- unlist(lapply(names(touched$matrices), function(mk) {
    vapply(touched$matrices[[mk]], function(nm) {
      full <- paste0(mk, ":", nm)
      x <- mat_ctr[[full]]
      if (is.null(x)) 0L else x
    }, integer(1))
  }), use.names = TRUE)

  list(axes = axes_snap, vectors = vecs_snap, matrices = mats_snap)
}

#' Canonicalise a query string.
#' @inheritParams get_query
#' @return The canonical query string (stable form; suitable for use as cache key).
#' @export
canonical_query <- function(query_string) {
  .canonicalise_ast(parse_query(query_string))
}

#' Test whether a query yields an axis entry vector.
#' @inheritParams get_query
#' @return Logical scalar.
#' @export
is_axis_query <- function(query_string) {
  ast <- parse_query(query_string)
  if (length(ast) == 0L) return(FALSE)
  last <- ast[[length(ast)]]
  last$op %in% c("Axis", "EndMask")
}

#' Return the axis name implied by a query, if any.
#' @inheritParams get_query
#' @return A single axis name, or `NA_character_` if the query references
#'   0 or 2+ axes.
#' @export
query_axis_name <- function(query_string) {
  ast <- parse_query(query_string)
  axes <- vapply(ast, function(n) {
    if (identical(n$op, "Axis")) n$axis_name else NA_character_
  }, character(1))
  axes <- axes[!is.na(axes)]
  if (length(axes) == 1L) axes else NA_character_
}

#' Return the dimensionality of a query's result.
#' @inheritParams get_query
#' @return 0L (scalar), 1L (vector / axis entries), 2L (matrix), or
#'   `NA_integer_` if the query is ill-formed.
#' @export
query_result_dimensions <- function(query_string) {
  ast <- parse_query(query_string)
  for (n in rev(ast)) {
    switch(n$op,
      LookupScalar   = return(0L),
      LookupVector   = return(1L),
      SquareRowIs    = ,
      SquareColumnIs = return(1L),
      LookupMatrix   = return(2L),
      ReduceToColumn = , ReduceToRow = return(1L),
      CountBy        = return(2L),
      Axis           = return(1L),
      NULL)
  }
  NA_integer_
}

#' Check whether a query can be evaluated against a daf without error.
#' @inheritParams get_query
#' @return Logical scalar. TRUE if `get_query(daf, query_string)` would
#'   succeed with a non-empty result; FALSE otherwise.
#' @export
has_query <- function(daf, query_string) {
  result <- tryCatch(get_query(daf, query_string), error = function(e) NULL)
  if (is.null(result)) return(FALSE)
  if (is.vector(result)) return(length(result) > 0L)
  if (is.matrix(result)) return(nrow(result) > 0L && ncol(result) > 0L)
  TRUE
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
