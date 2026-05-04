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
        v <- format_get_vector(daf, axis_name, nm)$value
        full_entries <- format_axis_array(daf, axis_name)$value
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
        df <- .apply_dataframe_columns(daf, df, axis_query = sprintf("@ %s", axis),
            columns = columns, error_label = sprintf("axis %s", sQuote(axis)))
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
#' @param columns Optional character vector of column names to include in
#'   the result. Defaults to all vectors on the resolved axis.
#' @param cache Logical; if `TRUE` (default), serve from the query cache.
#' @return A `data.frame` with axis-entry rownames.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "donor", c("d1", "d2"))
#' set_vector(d, "donor", "age", c(20L, 30L))
#' get_dataframe_query(d, "@ donor")
#' @seealso [get_dataframe()], [get_tidy()]
#' @export
get_dataframe_query <- function(daf, query, columns = NULL, cache = TRUE) {
    stopifnot("`daf` must be a DafReader" = is_daf(daf))
    df <- .get_dataframe_from_query(daf, query, cache = cache)
    if (!is.null(columns)) {
        df <- .apply_dataframe_columns(daf, df, axis_query = query,
            columns = columns, error_label = "query result")
    }
    df
}

# Apply a column selector to a per-axis data.frame. The selector is
# either a plain character vector of vector names (existing axis vectors)
# or a named character vector where names are output column names and
# values are query strings to evaluate against `daf` (Julia parity —
# `get_frame(daf, axis, ["mean_age" => "@ cell : age / metacell >> Mean"])`).
# The query must resolve to a vector on the same axis as the row labels.
.apply_dataframe_columns <- function(daf, df, axis_query, columns,
                                     error_label) {
    if (is.list(columns) && !is.null(names(columns))) {
        columns <- vapply(columns, as.character, character(1))
    }
    has_names <- !is.null(names(columns)) && any(nzchar(names(columns)))
    if (!has_names) {
        # Plain vector-name selection: behave like df[, columns].
        missing_cols <- setdiff(columns, colnames(df))
        if (length(missing_cols)) {
            stop(sprintf("columns not on %s: %s",
                error_label,
                paste(sQuote(missing_cols), collapse = ", ")
            ), call. = FALSE)
        }
        return(df[, columns, drop = FALSE])
    }
    # Mixed/named: each entry is `out_col -> query string`.
    # The row order must match df's existing rows (which are the axis
    # entries surviving any mask in the axis_query).
    row_names <- rownames(df)
    # Resolve the axis name from axis_query (e.g. "@ cell [ ... ]" -> "cell")
    # so that bare-`:`/`::` column queries can be auto-prefixed.
    axis_ast <- parse_query(axis_query)
    axis_name <- NULL
    for (n in axis_ast) {
        if (identical(n$op, "Axis")) {
            axis_name <- n$axis_name
            break
        }
    }
    out_cols <- vector("list", length(columns))
    names(out_cols) <- ifelse(nzchar(names(columns)), names(columns),
        unname(columns))
    for (i in seq_along(columns)) {
        q <- unname(columns[[i]])
        # Auto-prefix the axis when the column query is a bare lookup
        # (`: vec`, `:: m`, `. scalar`); Julia parity — column queries
        # without an explicit `@ axis` inherit the frame's axis context.
        # Also auto-resolve a bare property name (no leading operator) as
        # a vector on the frame's axis (the `list("age", ...)` shorthand
        # form mirrors Julia's `["age", "doublet" => ":is_doublet"]`).
        eff_q <- q
        trimmed <- trimws(q)
        first_char <- substr(trimmed, 1L, 1L)
        if (!is.null(axis_name) && !grepl("^@", trimmed)) {
            if (first_char %in% c(":", ".")) {
                eff_q <- sprintf("@ %s %s", axis_name, q)
            } else if (grepl("^[[:alnum:]_.]+$", trimmed)) {
                eff_q <- sprintf("@ %s : %s", axis_name, trimmed)
            }
        }
        result <- tryCatch(get_query(daf, eff_q),
            error = function(e) {
                stop(sprintf("invalid column query: %s for the %s",
                    q, error_label), call. = FALSE)
            })
        if (!is.atomic(result) || !is.null(dim(result))) {
            stop(sprintf("invalid column query: %s for the %s",
                q, error_label), call. = FALSE)
        }
        nm <- names(result)
        if (is.null(nm)) {
            if (length(result) != length(row_names)) {
                stop(sprintf("invalid column query: %s for the %s",
                    q, error_label), call. = FALSE)
            }
            out_cols[[i]] <- result
        } else {
            extra <- setdiff(nm, row_names)
            if (length(extra) > 0L) {
                stop(sprintf("invalid column query: %s for the %s",
                    q, error_label), call. = FALSE)
            }
            idx <- match(row_names, nm)
            if (anyNA(idx)) {
                stop(sprintf("invalid column query: %s for the %s",
                    q, error_label), call. = FALSE)
            }
            out_cols[[i]] <- result[idx]
        }
    }
    out_df <- as.data.frame(out_cols, row.names = row_names,
        stringsAsFactors = FALSE, optional = TRUE)
    out_df
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
