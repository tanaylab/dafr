#' @include query_eval.R query_parse.R query_class.R
NULL

# Public entry points: parse_query, get_query, has_query,
# is_axis_query, query_axis_name, query_result_dimensions,
# query_requires_relayout.

#' Evaluate a query against a daf reader.
#' @param daf A `DafReader`.
#' @param query_string A query string (character scalar) or a
#'   [DafrQuery] object produced by the query builders (e.g.
#'   `Axis("cell") |> LookupVector("donor")`).
#' @return A scalar, vector, matrix, names set, or NULL if missing.
#' @examples
#' d <- example_cells_daf()
#' get_query(d, ". organism")
#' head(get_query(d, "@ cell : donor"))
#' @export
get_query <- function(daf, query_string) {
    parts <- .get_query_dispatch(query_string)
    ast <- parts$ast
    canon <- parts$canonical
    key <- cache_key_query(canon)
    touched <- .collect_query_versions(daf, ast)
    stamp <- .snapshot_versions(daf, touched)
    cache_env <- S7::prop(daf, "cache")
    cached <- cache_lookup(cache_env, "query", key, stamp)
    if (!is.null(cached)) {
        return(cached)
    }
    value <- .eval_query(daf, ast)
    cache_store(cache_env, "query", key, value, stamp,
        size_bytes = as.numeric(object.size(value))
    )
    value
}

# Resolve either a character scalar or DafrQuery into (ast, canonical).
.get_query_dispatch <- function(q) {
    if (S7::S7_inherits(q, DafrQuery)) {
        list(ast = q@ast, canonical = q@canonical)
    } else if (is.character(q) && length(q) == 1L && !is.na(q)) {
        ast <- parse_query(q)
        list(ast = ast, canonical = .canonicalise_ast(ast))
    } else {
        stop("`query_string` must be a character scalar or DafrQuery", call. = FALSE)
    }
}

# Scan AST nodes and collect which axes/vectors/matrices the query reads.
# Returns list(axes, vectors, matrices) where vectors is a named list
# axis -> char vector of names, matrices is a named list "rows:cols" -> names.
.collect_query_versions <- function(daf, ast) {
    axes <- character(0)
    vecs <- list() # axis -> char vec of vector names
    mats <- list() # "rows:cols" -> char vec of matrix names
    scope_axis <- NULL
    rows_axis <- NULL
    cols_axis <- NULL
    two_axes <- FALSE

    for (n in ast) {
        switch(n$op,
            Axis = {
                axes <- c(axes, n$axis_name)
                if (isTRUE(two_axes)) {
                    # already in two-axis scope; ignore extra Axis nodes
                } else if (!is.null(scope_axis)) {
                    rows_axis <- scope_axis
                    cols_axis <- n$axis_name
                    two_axes <- TRUE
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
            BeginMask = ,
            BeginNegatedMask = ,
            AndMask = ,
            AndNegatedMask = ,
            OrMask = ,
            OrNegatedMask = ,
            XorMask = ,
            XorNegatedMask = {
                if (!is.null(n$property) && !is.null(scope_axis)) {
                    vecs[[scope_axis]] <- c(vecs[[scope_axis]], n$property)
                }
            },
            GroupBy = ,
            GroupRowsBy = ,
            GroupColumnsBy = ,
            CountBy = {
                if (!is.null(n$property)) {
                    ax <- if (isTRUE(two_axes)) {
                        if (identical(n$op, "GroupRowsBy")) {
                            rows_axis
                        } else if (identical(n$op, "GroupColumnsBy")) {
                            cols_axis
                        } else {
                            scope_axis
                        }
                    } else {
                        scope_axis
                    }
                    if (!is.null(ax)) vecs[[ax]] <- c(vecs[[ax]], n$property)
                }
            },
            NULL # ignore all other nodes
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
    ax_ctr <- S7::prop(daf, "axis_version_counter")
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
#' @examples
#' canonical_query("@ cell : donor")
#' @export
canonical_query <- function(query_string) {
    .canonicalise_ast(parse_query(query_string))
}

#' Test whether a query yields an axis entry vector.
#' @inheritParams get_query
#' @return Logical scalar.
#' @examples
#' is_axis_query("@ cell")
#' is_axis_query(". organism")
#' @export
is_axis_query <- function(query_string) {
    ast <- parse_query(query_string)
    if (length(ast) == 0L) {
        return(FALSE)
    }
    last <- ast[[length(ast)]]
    last$op %in% c("Axis", "EndMask")
}

#' Return the axis name implied by a query, if any.
#' @inheritParams get_query
#' @return A single axis name, or `NA_character_` if the query references
#'   0 or 2+ axes.
#' @examples
#' query_axis_name("@ cell : donor")
#' query_axis_name(". organism")
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
#' @examples
#' query_result_dimensions(". organism")
#' query_result_dimensions("@ cell : donor")
#' query_result_dimensions("@ cell @ gene :: UMIs")
#' @export
query_result_dimensions <- function(query_string) {
    ast <- parse_query(query_string)
    for (n in rev(ast)) {
        switch(n$op,
            LookupScalar = return(0L),
            LookupVector = return(1L),
            SquareRowIs = ,
            SquareColumnIs = return(1L),
            LookupMatrix = return(2L),
            ReduceToColumn = ,
            ReduceToRow = return(1L),
            CountBy = return(2L),
            Axis = return(1L),
            NULL
        )
    }
    NA_integer_
}

#' Does evaluating this query require a matrix relayout (transpose)?
#'
#' Walks the parsed AST and returns `TRUE` if any `LookupMatrix` node
#' would read a matrix stored with axis order different from the order
#' implied by the surrounding `@ rows @ cols` scopes, or if a
#' `ReduceToColumn`/`ReduceToRow` would force a relayout.
#'
#' @inheritParams get_query
#' @return Logical scalar.
#' @examples
#' d <- example_cells_daf()
#' query_requires_relayout(d, "@ cell @ gene :: UMIs") # stored order → FALSE
#' query_requires_relayout(d, "@ gene @ cell :: UMIs") # swapped → TRUE
#' @export
query_requires_relayout <- function(daf, query_string) {
    ast <- parse_query(query_string)
    rows_axis <- NULL
    cols_axis <- NULL
    two_axes <- FALSE
    scope_axis <- NULL
    for (n in ast) {
        switch(n$op,
            Axis = {
                if (isTRUE(two_axes)) {
                    # ignore further Axis nodes inside two-axis scope
                } else if (!is.null(scope_axis)) {
                    rows_axis <- scope_axis
                    cols_axis <- n$axis_name
                    two_axes <- TRUE
                } else {
                    scope_axis <- n$axis_name
                }
            },
            LookupMatrix = {
                if (isTRUE(two_axes) && !is.null(n$name)) {
                    if (!format_has_matrix(daf, rows_axis, cols_axis, n$name) &&
                        format_has_matrix(daf, cols_axis, rows_axis, n$name)) {
                        return(TRUE)
                    }
                }
            },
            ReduceToColumn = ,
            ReduceToRow = {
                if (isTRUE(two_axes)) {
                    # A column-reduction of a (rows, cols) matrix stored
                    # as (cols, rows) requires relayout to iterate by column.
                    return(
                        !format_has_matrix(daf, rows_axis, cols_axis,
                            last_matrix_name(ast)
                        ) &&
                            format_has_matrix(daf, cols_axis, rows_axis,
                                last_matrix_name(ast)
                            )
                    )
                }
            },
            NULL
        )
    }
    FALSE
}

# Find the most recent LookupMatrix $name in an AST, for reduction
# dispatch in query_requires_relayout. Returns NA_character_ if absent.
last_matrix_name <- function(ast) {
    for (n in rev(ast)) {
        if (identical(n$op, "LookupMatrix") && !is.null(n$name)) {
            return(n$name)
        }
    }
    NA_character_
}

#' Check whether a query can be evaluated against a daf without error.
#' @inheritParams get_query
#' @return Logical scalar. TRUE if `get_query(daf, query_string)` would
#'   succeed with a non-empty result; FALSE otherwise.
#' @examples
#' d <- example_cells_daf()
#' has_query(d, "@ cell : donor")
#' has_query(d, "@ cell : nonexistent_property")
#' @export
has_query <- function(daf, query_string) {
    # Validate input shape up-front; evaluation errors still fall through to FALSE.
    .get_query_dispatch(query_string)
    result <- tryCatch(get_query(daf, query_string), error = function(e) NULL)
    if (is.null(result)) {
        return(FALSE)
    }
    if (is.vector(result)) {
        return(length(result) > 0L)
    }
    if (is.matrix(result)) {
        return(nrow(result) > 0L && ncol(result) > 0L)
    }
    TRUE
}

# `daf[q]` is shorthand for `get_query(daf, q)`. Accepts either a
# character-scalar query string or a `DafrQuery`. S7 registers the
# method against the base `[` generic, which makes it discoverable via
# normal S3 dispatch -- no `@export` tag needed (in fact roxygen would
# generate an invalid `export()` line for this non-function name).
S7::method(`[`, DafReader) <- function(x, i) {
    get_query(x, i)
}

