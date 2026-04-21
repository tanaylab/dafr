#' @include classes.R queries.R format_api.R
NULL

# Sentinel constants for wildcard view specifications.
#' Wildcard for all axes.
#' @export
ALL_AXES <- "*"
#' Wildcard for all scalars.
#' @export
ALL_SCALARS <- "*"
#' Wildcard for all vectors.
#' @export
ALL_VECTORS <- c("*", "*")
#' Wildcard for all matrices.
#' @export
ALL_MATRICES <- c("*", "*", "*")

# Default view specs (composed below).
# Scalars/axes use a named-list form  list("*" = "=")  so parse_view_item
# returns key = "*".  Vectors/matrices use the positional two-element form
# list(c("*","*"), "=") / list(c("*","*","*"), "=") so the key is the vector.

#' Default view spec: expose all axes as-is.
#'
#' A pre-built `axes` override list for `viewer()` that exposes every axis of
#' the base daf unchanged. Equivalent to `list(list(ALL_AXES, "="))`.
#' @seealso [viewer()], [ALL_AXES]
#' @export
VIEW_ALL_AXES <- setNames(list("="), ALL_AXES)

#' Default view spec: expose all scalars as-is.
#'
#' A pre-built `data` item for `viewer()` that exposes every scalar of the base
#' daf unchanged. Equivalent to `list(list(ALL_SCALARS, "="))`.
#' @seealso [viewer()], [ALL_SCALARS]
#' @export
VIEW_ALL_SCALARS <- setNames(list("="), ALL_SCALARS)

#' Default view spec: expose all vectors as-is.
#'
#' A pre-built `data` item for `viewer()` that exposes every vector of every
#' axis of the base daf unchanged.
#' @seealso [viewer()], [ALL_VECTORS]
#' @export
VIEW_ALL_VECTORS <- list(list(ALL_VECTORS, "="))

#' Default view spec: expose all matrices as-is.
#'
#' A pre-built `data` item for `viewer()` that exposes every matrix of the base
#' daf unchanged.
#' @seealso [viewer()], [ALL_MATRICES]
#' @export
VIEW_ALL_MATRICES <- list(list(ALL_MATRICES, "="))

#' Default view spec: expose all data (scalars + vectors + matrices) as-is.
#'
#' A convenience list combining [VIEW_ALL_SCALARS], [VIEW_ALL_VECTORS], and
#' [VIEW_ALL_MATRICES]. Pass as the `data` argument to `viewer()` to expose
#' every data item from the base daf unchanged.
#' @seealso [viewer()], [VIEW_ALL_SCALARS], [VIEW_ALL_VECTORS],
#'   [VIEW_ALL_MATRICES]
#' @export
VIEW_ALL_DATA <- list(VIEW_ALL_SCALARS, VIEW_ALL_VECTORS, VIEW_ALL_MATRICES)

#' A read-only view over a base daf.
#'
#' A ViewDaf carries a reference to a base `DafReader` and a dictionary of
#' per-name query overrides (axes / scalars / vectors / matrices). Reads on
#' the view rewrite into queries against the base; no data is copied.
#'
#' Users should construct `ViewDaf` objects via [viewer()] rather than calling
#' the constructor directly.
#'
#' @param name Character scalar; name of the view.
#' @param internal Internal environment (created by `new_internal_env()`).
#' @param cache Cache environment (created by `new_cache_env()`).
#' @param axis_version_counter Counter environment for axis version tracking.
#' @param vector_version_counter Counter environment for vector version tracking.
#' @param matrix_version_counter Counter environment for matrix version tracking.
#' @param base Base `DafReader` this view wraps.
#' @param view_axes Named list mapping view axis names to query strings.
#' @param view_axis_renames Named list mapping view axis names to base axis
#'   names.
#' @param view_axis_indices Named list mapping view axis names to 1-based integer
#'   vectors of positions within each base axis that the view exposes.
#' @param view_scalars Named list mapping view scalar names to query strings.
#' @param view_vectors Named list mapping `"axis|name"` keys to override specs.
#' @param view_matrices Named list mapping `"rows|cols|name"` keys to override
#'   specs.
#' @seealso [viewer()]
#' @export
ViewDaf <- S7::new_class(
    name = "ViewDaf",
    package = "dafr",
    parent = DafReadOnly,
    properties = list(
        base              = DafReader,
        view_axes         = S7::class_list,
        view_axis_renames = S7::class_list,
        view_axis_indices = S7::class_list,
        view_scalars      = S7::class_list,
        view_vectors      = S7::class_list,
        view_matrices     = S7::class_list
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
#' @examples
#' d <- example_cells_daf()
#' # NULL query drops an axis from the view:
#' v <- viewer(d, axes = list(list(ALL_AXES, "="), list("gene", NULL)))
#' axes_set(v)
#' vectors_set(v, "cell")
#' @export
viewer <- function(daf, name = NULL, axes = NULL, data = NULL) {
    if (is.null(name)) name <- paste0(S7::prop(daf, "name"), ".view")
    view_axes    <- .resolve_view_axes(daf, axes)
    view_renames <- .resolve_view_axis_renames(daf, view_axes)
    view_indices <- .resolve_view_axis_indices(daf, view_axes, view_renames)
    ViewDaf(
        name                    = name,
        internal                = new_internal_env(),
        cache                   = S7::prop(daf, "cache"),
        axis_version_counter    = S7::prop(daf, "axis_version_counter"),
        vector_version_counter  = S7::prop(daf, "vector_version_counter"),
        matrix_version_counter  = S7::prop(daf, "matrix_version_counter"),
        base                    = daf,
        view_axes               = view_axes,
        view_axis_renames       = view_renames,
        view_axis_indices       = view_indices,
        view_scalars            = .resolve_view_scalars(daf, data),
        view_vectors            = .resolve_view_vectors(daf, data, view_renames),
        view_matrices           = .resolve_view_matrices(daf, data, view_renames)
    )
}

.resolve_view_axes <- function(daf, axes) {
    all_axes <- format_axes_set(daf)
    if (is.null(axes)) {
        return(setNames(rep(list("="), length(all_axes)), all_axes))
    }
    out <- list()
    for (item in axes) {
        parsed <- .parse_view_item(item)
        name <- parsed$key
        query <- parsed$value
        if (identical(name, ALL_AXES)) {
            if (is.null(query)) {
                out <- list() # hide everything
            } else {
                for (a in all_axes) out[[a]] <- query
            }
        } else {
            if (is.null(query)) {
                out[[name]] <- NULL
            } else {
                out[[name]] <- query
            }
        }
    }
    out
}

.resolve_view_axis_renames <- function(daf, view_axes) {
    out <- list()
    for (view_name in names(view_axes)) {
        q <- view_axes[[view_name]]
        if (identical(q, "=") || identical(q, view_name)) {
            out[[view_name]] <- view_name
        } else {
            base_axis <- query_axis_name(q)
            if (is.na(base_axis)) {
                stop(sprintf(
                    "view axis %s: cannot infer base axis from query %s",
                    sQuote(view_name), sQuote(q)
                ), call. = FALSE)
            }
            out[[view_name]] <- base_axis
        }
    }
    out
}

.resolve_view_axis_indices <- function(daf, view_axes, renames) {
    out <- list()
    for (view_name in names(view_axes)) {
        q <- view_axes[[view_name]]
        base_axis <- renames[[view_name]]
        base_entries <- format_axis_array(daf, base_axis)
        if (identical(q, "=") || identical(q, view_name)) {
            out[[view_name]] <- seq_along(base_entries)
        } else {
            view_entries <- get_query(daf, q)
            idx <- match(view_entries, base_entries)
            if (anyNA(idx)) {
                stop(sprintf(
                    "view axis %s: entry %s not in base axis %s",
                    sQuote(view_name),
                    sQuote(view_entries[is.na(idx)][[1L]]),
                    sQuote(base_axis)
                ), call. = FALSE)
            }
            out[[view_name]] <- idx
        }
    }
    out
}

.resolve_view_scalars <- function(daf, data) {
    all_scalars <- format_scalars_set(daf)
    out <- list()
    if (is.null(data)) {
        return(setNames(as.list(all_scalars), all_scalars))
    }
    for (item in .flatten_view_data(data)) {
        parsed <- .parse_view_item(item)
        if (is.character(parsed$key) && length(parsed$key) == 1L) {
            name <- parsed$key
            query <- parsed$value
            if (identical(name, ALL_SCALARS)) {
                if (is.null(query)) {
                    out <- list()
                } else if (identical(query, "=")) {
                    for (s in all_scalars) out[[s]] <- s
                } else {
                    for (s in all_scalars) out[[s]] <- query
                }
            } else {
                if (is.null(query)) {
                    out[[name]] <- NULL
                } else {
                    out[[name]] <- query
                }
            }
        }
    }
    out
}

.resolve_view_vectors <- function(daf, data, renames) {
    out <- list()
    # Seed every renamed axis with every base vector visible on its base axis.
    for (view_axis in names(renames)) {
        base_axis <- renames[[view_axis]]
        for (v in format_vectors_set(daf, base_axis)) {
            out[[paste(view_axis, v, sep = "|")]] <- list(
                view_axis = view_axis,
                base_axis = base_axis,
                name = v,
                query = "="
            )
        }
    }
    if (is.null(data)) {
        return(out)
    }
    for (item in .flatten_view_data(data)) {
        parsed <- .parse_view_item(item)
        if (is.character(parsed$key) && length(parsed$key) == 2L) {
            a <- parsed$key[[1L]]
            v <- parsed$key[[2L]]
            q <- parsed$value
            base_axis <- renames[[a]] %||% a
            if (identical(a, "*") && identical(v, "*")) {
                if (is.null(q)) {
                    out <- list()
                } else if (identical(q, "=")) {
                    # identity — already the default, no-op
                } else {
                    for (k in names(out)) out[[k]]$query <- q
                }
            } else {
                key <- paste(a, v, sep = "|")
                if (is.null(q)) {
                    out[[key]] <- NULL
                } else {
                    out[[key]] <- list(
                        view_axis = a,
                        base_axis = base_axis,
                        name = v,
                        query = q
                    )
                }
            }
        }
    }
    out
}

.resolve_view_matrices <- function(daf, data, renames) {
    out <- list()
    for (rv in names(renames)) {
        rb <- renames[[rv]]
        for (cv in names(renames)) {
            cb <- renames[[cv]]
            for (m in format_matrices_set(daf, rb, cb)) {
                out[[paste(rv, cv, m, sep = "|")]] <- list(
                    view_rows = rv,
                    view_cols = cv,
                    base_rows = rb,
                    base_cols = cb,
                    name = m,
                    query = "="
                )
            }
        }
    }
    if (is.null(data)) {
        return(out)
    }
    for (item in .flatten_view_data(data)) {
        parsed <- .parse_view_item(item)
        if (is.character(parsed$key) && length(parsed$key) == 3L) {
            rr <- parsed$key[[1L]]
            cc <- parsed$key[[2L]]
            nn <- parsed$key[[3L]]
            q <- parsed$value
            rb <- renames[[rr]] %||% rr
            cb <- renames[[cc]] %||% cc
            if (rr == "*" && cc == "*" && nn == "*") {
                if (is.null(q)) out <- list()
                # identity "=" is default; no-op
            } else {
                key <- paste(rr, cc, nn, sep = "|")
                if (is.null(q)) {
                    out[[key]] <- NULL
                } else {
                    out[[key]] <- list(
                        view_rows = rr,
                        view_cols = cc,
                        base_rows = rb,
                        base_cols = cb,
                        name = nn,
                        query = q
                    )
                }
            }
        }
    }
    out
}

# Helpers

.parse_view_item <- function(item) {
    # Accept either list(key, value) two-element or single-named list(key = value).
    if (is.list(item) && length(item) == 2L && is.null(names(item))) {
        return(list(key = item[[1L]], value = item[[2L]]))
    }
    if (is.list(item) && length(item) == 1L && !is.null(names(item))) {
        return(list(key = names(item), value = item[[1L]]))
    }
    stop("view item must be list(key, value) or list(name = value)", call. = FALSE)
}

.flatten_view_data <- function(data) {
    flat <- list()
    for (item in data) {
        if (is.list(item) && length(item) > 0L &&
            (is.list(item[[1L]]) ||
                (!is.null(names(item[[1L]])) && length(item[[1L]]) == 1L))) {
            flat <- c(flat, .flatten_view_data(item))
        } else {
            flat <- c(flat, list(item))
        }
    }
    flat
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
    if (is.null(override)) {
        return(NULL)
    }
    if (identical(override$query, "=")) {
        return(sprintf("@ %s : %s", override$base_axis, override$name))
    }
    override$query
}

.view_query_for_matrix <- function(view, rows_axis, columns_axis, name) {
    key <- paste(rows_axis, columns_axis, name, sep = "|")
    override <- view@view_matrices[[key]]
    if (is.null(override)) {
        return(NULL)
    }
    if (identical(override$query, "=")) {
        return(sprintf(
            "@ %s @ %s :: %s",
            override$base_rows, override$base_cols, override$name
        ))
    }
    override$query
}

# --- format_* dispatch --------------------------------------------------

S7::method(
    format_has_scalar,
    list(ViewDaf, S7::class_character)
) <- function(daf, name) {
    if (!(name %in% names(daf@view_scalars))) {
        return(FALSE)
    }
    q_str <- .view_query_for_scalar(daf, name)
    has_query(daf@base, q_str)
}

S7::method(
    format_get_scalar,
    list(ViewDaf, S7::class_character)
) <- function(daf, name) {
    get_query(daf@base, .view_query_for_scalar(daf, name))
}

S7::method(format_scalars_set, ViewDaf) <- function(daf) {
    nms <- names(daf@view_scalars)
    if (is.null(nms)) {
        return(character(0L))
    }
    sort(nms, method = "radix")
}

S7::method(
    format_has_axis,
    list(ViewDaf, S7::class_character)
) <- function(daf, axis) {
    !is.null(daf@view_axes[[axis]])
}

S7::method(format_axes_set, ViewDaf) <- function(daf) {
    nms <- names(daf@view_axes)
    if (is.null(nms)) {
        return(character(0L))
    }
    sort(nms, method = "radix")
}

S7::method(
    format_axis_length,
    list(ViewDaf, S7::class_character)
) <- function(daf, axis) {
    length(daf@view_axis_indices[[axis]])
}

S7::method(
    format_axis_array,
    list(ViewDaf, S7::class_character)
) <- function(daf, axis) {
    idx <- daf@view_axis_indices[[axis]]
    base_axis <- daf@view_axis_renames[[axis]]
    format_axis_array(daf@base, base_axis)[idx]
}

S7::method(
    format_has_vector,
    list(ViewDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    key <- paste(axis, name, sep = "|")
    !is.null(daf@view_vectors[[key]])
}

S7::method(
    format_vectors_set,
    list(ViewDaf, S7::class_character)
) <- function(daf, axis) {
    keys <- names(daf@view_vectors) %||% character(0)
    prefix <- paste0(axis, "|")
    sub(prefix, "", keys[startsWith(keys, prefix)], fixed = TRUE)
}

S7::method(
    format_get_vector,
    list(ViewDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    q_str <- .view_query_for_vector(daf, axis, name)
    if (is.null(q_str)) {
        stop(sprintf(
            "no vector %s on view axis %s",
            sQuote(name), sQuote(axis)
        ), call. = FALSE)
    }
    raw <- get_query(daf@base, q_str)
    idx <- daf@view_axis_indices[[axis]]
    raw[idx]
}

S7::method(
    format_has_matrix,
    list(ViewDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    key <- paste(rows_axis, columns_axis, name, sep = "|")
    !is.null(daf@view_matrices[[key]])
}

S7::method(
    format_matrices_set,
    list(ViewDaf, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis) {
    keys <- names(daf@view_matrices) %||% character(0)
    prefix <- paste(rows_axis, columns_axis, "", sep = "|")
    sub(prefix, "", keys[startsWith(keys, prefix)], fixed = TRUE)
}

S7::method(
    format_get_matrix,
    list(ViewDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    q_str <- .view_query_for_matrix(daf, rows_axis, columns_axis, name)
    if (is.null(q_str)) {
        stop(sprintf(
            "no matrix %s on view axes (%s, %s)",
            sQuote(name), sQuote(rows_axis), sQuote(columns_axis)
        ), call. = FALSE)
    }
    raw <- get_query(daf@base, q_str)
    r_idx <- daf@view_axis_indices[[rows_axis]]
    c_idx <- daf@view_axis_indices[[columns_axis]]
    raw[r_idx, c_idx, drop = FALSE]
}
