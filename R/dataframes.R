#' @include classes.R format_api.R queries.R
NULL

# ---- Internals -------------------------------------------------------------

# Extracted body of the old get_frame, now driving both
# get_dataframe_query and get_dataframe. Kept private.
.get_dataframe_from_query <- function(daf, query_string, cache = TRUE) {
    # Cache key: route through the existing query cache if requested.
    if (isTRUE(cache)) {
        ast <- parse_query(query_string)
        canon <- .canonicalise_ast(ast)
        key <- cache_key_query(canon)
        touched <- .collect_query_versions(daf, ast)
        stamp <- .snapshot_versions(daf, touched)
        cache_env <- S7::prop(daf, "cache")
        cached <- cache_lookup(cache_env, "query", key, stamp)
        if (!is.null(cached) && is.data.frame(cached)) {
            return(cached)
        }
    }
    axis_ast <- parse_query(query_string)
    state <- list(kind = "init", value = NULL, if_missing = NULL)
    for (node in axis_ast) state <- .apply_node(node, state, daf)
    if (!identical(state$kind, "axis")) {
        stop("query did not resolve to an axis", call. = FALSE)
    }
    entries <- state$value
    axis_name <- state$axis
    columns <- format_vectors_set(daf, axis_name)
    cols <- lapply(columns, function(nm) {
        v <- format_get_vector(daf, axis_name, nm)
        full_entries <- format_axis_array(daf, axis_name)
        idx <- match(entries, full_entries)
        v[idx]
    })
    names(cols) <- columns
    df <- as.data.frame(cols,
        row.names = entries,
        stringsAsFactors = FALSE, optional = TRUE
    )
    if (isTRUE(cache)) {
        cache_store(cache_env, "query", key, df, stamp,
            size_bytes = as.numeric(object.size(df))
        )
    }
    df
}

# ---- Public exports --------------------------------------------------------

#' Extract vectors on an axis as a `data.frame`.
#'
#' Returns a `data.frame` with one column per vector on `axis`, rows
#' named by the axis entries. When `columns` is `NULL`, all vectors
#' defined on `axis` are included.
#'
#' @param daf A [DafReader].
#' @param axis Axis name.
#' @param columns Optional character vector of vector names to
#'   include. Defaults to all vectors on `axis`.
#' @param cache Logical; if `TRUE` (default), the result is memoised
#'   in the query cache and served on repeat calls.
#' @return A `data.frame`.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "cell", c("c1", "c2"))
#' set_vector(d, "cell", "donor", c("A", "B"))
#' get_dataframe(d, "cell")
#' @seealso [get_dataframe_query()], [get_tidy()]
#' @export
get_dataframe <- function(daf, axis, columns = NULL, cache = TRUE) {
    stopifnot("`daf` must be a DafReader" = is_daf(daf))
    df <- .get_dataframe_from_query(daf, sprintf("@ %s", axis), cache = cache)
    if (!is.null(columns)) {
        missing_cols <- setdiff(columns, colnames(df))
        if (length(missing_cols)) {
            stop(sprintf("columns not on axis %s: %s",
                sQuote(axis),
                paste(sQuote(missing_cols), collapse = ", ")
            ), call. = FALSE)
        }
        df <- df[, columns, drop = FALSE]
    }
    df
}

#' Extract an axis-resolving query's result as a `data.frame`.
#'
#' The query-string counterpart of [get_dataframe()]. The query must
#' resolve to an axis (possibly mask-filtered).
#'
#' @param daf A [DafReader].
#' @param query A query string resolving to an axis.
#' @param cache Logical; if `TRUE` (default), serve from the query cache.
#' @return A `data.frame` with axis-entry rownames.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "donor", c("d1", "d2"))
#' set_vector(d, "donor", "age", c(20L, 30L))
#' get_dataframe_query(d, "@ donor")
#' @seealso [get_dataframe()], [get_tidy()]
#' @export
get_dataframe_query <- function(daf, query, cache = TRUE) {
    stopifnot("`daf` must be a DafReader" = is_daf(daf))
    .get_dataframe_from_query(daf, query, cache = cache)
}

#' Pivot axis vectors into a tidy long-format tibble.
#'
#' Requires `tidyr` and `tibble`; errors with an install hint if
#' either is missing.
#'
#' @inheritParams get_dataframe
#' @param ... Passed to [tidyr::pivot_longer()].
#' @return A `tibble` with columns `name`, `key`, `value`.
#' @examples
#' if (requireNamespace("tidyr", quietly = TRUE) &&
#'     requireNamespace("tibble", quietly = TRUE)) {
#'     d <- memory_daf()
#'     add_axis(d, "cell", c("c1", "c2"))
#'     set_vector(d, "cell", "donor", c("A", "B"))
#'     get_tidy(d, "cell")
#' }
#' @seealso [get_dataframe()], [get_dataframe_query()]
#' @export
get_tidy <- function(daf, axis, columns = NULL, cache = TRUE, ...) {
    rlang::check_installed(
        c("tidyr", "tibble"),
        reason = "for `get_tidy()`"
    )
    df <- get_dataframe(daf, axis, columns = columns, cache = cache)
    df$name <- rownames(df)
    rownames(df) <- NULL
    tib <- tibble::as_tibble(df)
    tidyr::pivot_longer(tib, -"name",
        names_to = "key", values_to = "value", ...
    )
}
