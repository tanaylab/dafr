#' @include classes.R queries.R format_api.R
NULL

# Sentinel constants for wildcard view specifications.
#' Wildcard for all axes.
#' @examples
#' d <- example_cells_daf()
#' v <- viewer(d, axes = list(list(ALL_AXES, "=")))
#' axes_set(v)
#' @export
ALL_AXES <- "*"
#' Wildcard for all scalars.
#' @examples
#' d <- memory_daf()
#' set_scalar(d, "organism", "human")
#' v <- viewer(d, data = list(list(ALL_SCALARS, "=")))
#' scalars_set(v)
#' @export
ALL_SCALARS <- "*"
#' Wildcard for all vectors.
#' @examples
#' d <- example_cells_daf()
#' v <- viewer(d, data = list(list(ALL_VECTORS, "=")))
#' vectors_set(v, "cell")
#' @export
ALL_VECTORS <- c("*", "*")
#' Wildcard for all matrices.
#' @examples
#' d <- example_cells_daf()
#' v <- viewer(d, data = list(list(ALL_MATRICES, "=")))
#' matrices_set(v, "cell", "gene")
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
#' @examples
#' d <- example_cells_daf()
#' v <- viewer(d, axes = list(VIEW_ALL_AXES))
#' axes_set(v)
#' @export
VIEW_ALL_AXES <- setNames(list("="), ALL_AXES)

#' Default view spec: expose all scalars as-is.
#'
#' A pre-built `data` item for `viewer()` that exposes every scalar of the base
#' daf unchanged. Equivalent to `list(list(ALL_SCALARS, "="))`.
#' @seealso [viewer()], [ALL_SCALARS]
#' @examples
#' d <- memory_daf()
#' set_scalar(d, "organism", "human")
#' v <- viewer(d, data = list(VIEW_ALL_SCALARS))
#' scalars_set(v)
#' @export
VIEW_ALL_SCALARS <- setNames(list("="), ALL_SCALARS)

#' Default view spec: expose all vectors as-is.
#'
#' A pre-built `data` item for `viewer()` that exposes every vector of every
#' axis of the base daf unchanged.
#' @seealso [viewer()], [ALL_VECTORS]
#' @examples
#' d <- example_cells_daf()
#' v <- viewer(d, data = VIEW_ALL_VECTORS)
#' vectors_set(v, "cell")
#' @export
VIEW_ALL_VECTORS <- list(list(ALL_VECTORS, "="))

#' Default view spec: expose all matrices as-is.
#'
#' A pre-built `data` item for `viewer()` that exposes every matrix of the base
#' daf unchanged.
#' @seealso [viewer()], [ALL_MATRICES]
#' @examples
#' d <- example_cells_daf()
#' v <- viewer(d, data = VIEW_ALL_MATRICES)
#' matrices_set(v, "cell", "gene")
#' @export
VIEW_ALL_MATRICES <- list(list(ALL_MATRICES, "="))

#' Default view spec: expose all data (scalars + vectors + matrices) as-is.
#'
#' A convenience list combining [VIEW_ALL_SCALARS], [VIEW_ALL_VECTORS], and
#' [VIEW_ALL_MATRICES]. Pass as the `data` argument to `viewer()` to expose
#' every data item from the base daf unchanged.
#' @seealso [viewer()], [VIEW_ALL_SCALARS], [VIEW_ALL_VECTORS],
#'   [VIEW_ALL_MATRICES]
#' @examples
#' d <- example_cells_daf()
#' v <- viewer(d, data = VIEW_ALL_DATA)
#' scalars_set(v)
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
#' @examples
#' d <- example_cells_daf()
#' v <- viewer(d)
#' inherits(v, "dafr::ViewDaf")
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
        view_scalars            = .resolve_view_scalars(daf, data, view_name = name),
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
        base_entries <- format_axis_array(daf, base_axis)$value
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

.resolve_view_scalars <- function(daf, data, view_name = NULL) {
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
                # Julia parity: wildcard scalar query must be "=" or
                # nothing (NULL). Any other query string is an error
                # (DataAxesFormats.jl/src/views.jl `invalid wildcard
                # scalar query: ...`).
                if (is.null(query)) {
                    out <- list()
                } else if (identical(query, "=")) {
                    for (s in all_scalars) out[[s]] <- s
                } else {
                    stop(sprintf(
                        "invalid wildcard scalar query: %s\nquery for wildcard must be one of: \"=\", nothing\nfor the view: %s\nof the daf data: %s",
                        as.character(query), view_name %||% "view!",
                        S7::prop(daf, "name")
                    ), call. = FALSE)
                }
            } else {
                if (is.null(query)) {
                    out[[name]] <- NULL
                } else if (identical(query, "=")) {
                    out[[name]] <- name
                } else {
                    # Julia parity (V7): a scalar-view query must
                    # produce a scalar shape. Validate now via
                    # query_result_dimensions; reject vector- /
                    # matrix-producing queries here so get_scalar()
                    # later doesn't surface a vector silently.
                    dims <- tryCatch(
                        query_result_dimensions(query),
                        error = function(e) -1
                    )
                    if (!is.numeric(dims) || dims != 0L) {
                        stop(sprintf(
                            "invalid scalar query: %s\nquery does not produce a scalar shape\nfor the scalar property: %s\nfor the view: %s\nof the daf data: %s",
                            as.character(query), name,
                            view_name %||% "view!",
                            S7::prop(daf, "name")
                        ), call. = FALSE)
                    }
                    out[[name]] <- query
                }
            }
        }
    }
    out
}

.resolve_view_vectors <- function(daf, data, renames) {
    # Julia parity (V3): the viewer is a strict include list. We seed
    # all-base-vectors ONLY when the data list is NULL (defaulted),
    # matching Julia's "default = expose everything" behaviour while
    # an explicit `data = list(...)` is treated as include-only.
    seed_all <- function() {
        seeded <- list()
        for (view_axis in names(renames)) {
            base_axis <- renames[[view_axis]]
            for (v in format_vectors_set(daf, base_axis)) {
                seeded[[paste(view_axis, v, sep = "|")]] <- list(
                    view_axis = view_axis,
                    base_axis = base_axis,
                    name = v,
                    query = "="
                )
            }
        }
        seeded
    }
    if (is.null(data) || length(data) == 0L) {
        return(seed_all())
    }
    out <- list()
    for (item in .flatten_view_data(data)) {
        parsed <- .parse_view_item(item)
        if (is.character(parsed$key) && length(parsed$key) == 2L) {
            a <- parsed$key[[1L]]
            v <- parsed$key[[2L]]
            q <- parsed$value
            base_axis <- renames[[a]] %||% a
            if (identical(a, "*") && identical(v, "*")) {
                # Julia parity: wildcard vector query must be "=" or
                # nothing. `=` means "expose all base vectors" (now
                # explicit, since the strict-include policy means we
                # only seed when the user opts in).
                if (is.null(q)) {
                    out <- list()
                } else if (identical(q, "=")) {
                    out <- seed_all()
                } else {
                    stop(sprintf(
                        "invalid wildcard vector query: %s\nquery for wildcard must be one of: \"=\", nothing\nfor the vector property: *\nfor the vector axis: *\nfor the view: %s\nof the daf data: %s",
                        as.character(q), "view!",
                        S7::prop(daf, "name")
                    ), call. = FALSE)
                }
            } else if (identical(v, "*")) {
                # Wildcard property name on a specific axis: must be "=" or NULL.
                base_a <- renames[[a]] %||% a
                if (is.null(q)) {
                    # Hide all vectors on this axis.
                    for (k in names(out)) {
                        if (identical(out[[k]]$view_axis, a)) out[[k]] <- NULL
                    }
                } else if (identical(q, "=")) {
                    # Identity already the default.
                } else {
                    stop(sprintf(
                        "invalid wildcard vector query: %s\nquery for wildcard must be one of: \"=\", nothing\nfor the vector property: *\nfor the vector axis: %s\nfor the view: %s\nof the daf data: %s",
                        as.character(q), a, "view!",
                        S7::prop(daf, "name")
                    ), call. = FALSE)
                }
            } else if (identical(a, "*")) {
                # Wildcard axis with specific property name: must be "=" or NULL.
                if (is.null(q)) {
                    for (k in names(out)) {
                        if (identical(out[[k]]$name, v)) out[[k]] <- NULL
                    }
                } else if (identical(q, "=")) {
                    # Identity already the default for matching props.
                } else {
                    stop(sprintf(
                        "invalid wildcard vector query: %s\nquery for wildcard must be one of: \"=\", nothing\nfor the vector property: %s\nfor the vector axis: *\nfor the view: %s\nof the daf data: %s",
                        as.character(q), v, "view!",
                        S7::prop(daf, "name")
                    ), call. = FALSE)
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
    # Julia parity (V3): strict include list when explicit `data` is
    # provided; expose-all only when defaulted.
    seed_all <- function() {
        seeded <- list()
        for (rv in names(renames)) {
            rb <- renames[[rv]]
            for (cv in names(renames)) {
                cb <- renames[[cv]]
                for (m in format_matrices_set(daf, rb, cb)) {
                    seeded[[paste(rv, cv, m, sep = "|")]] <- list(
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
        seeded
    }
    if (is.null(data) || length(data) == 0L) {
        return(seed_all())
    }
    out <- list()
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
                # Julia parity: wildcard matrix query must be "=" or nothing.
                if (is.null(q)) {
                    out <- list()
                } else if (identical(q, "=")) {
                    out <- seed_all()
                } else {
                    stop(sprintf(
                        "invalid wildcard matrix query: %s\nquery for wildcard must be one of: \"=\", nothing\nfor the matrix property: *\nfor the rows axis: *\nfor the columns axis: *\nfor the view: %s\nof the daf data: %s",
                        as.character(q), "view!",
                        S7::prop(daf, "name")
                    ), call. = FALSE)
                }
            } else if (rr == "*" || cc == "*" || nn == "*") {
                # Partial wildcards: must be "=" or NULL.
                if (is.null(q) || identical(q, "=")) {
                    # Apply at runtime via existing matching paths -
                    # for now, error if not the simple "=" case where
                    # every wildcard slot keeps the existing setup.
                    if (identical(q, "=")) {
                        # Only "=" pass-through is meaningful for partial
                        # wildcards; keep existing entries as-is.
                    } else {
                        # NULL: hide matrices matching this pattern.
                        for (k in names(out)) {
                            entry <- out[[k]]
                            keep <- TRUE
                            if (rr != "*" && !identical(entry$view_rows, rr)) keep <- TRUE
                            else if (cc != "*" && !identical(entry$view_cols, cc)) keep <- TRUE
                            else if (nn != "*" && !identical(entry$name, nn)) keep <- TRUE
                            else keep <- FALSE
                            if (!keep) out[[k]] <- NULL
                        }
                    }
                } else {
                    rrs <- if (rr == "*") "*" else rr
                    ccs <- if (cc == "*") "*" else cc
                    nns <- if (nn == "*") "*" else nn
                    stop(sprintf(
                        "invalid wildcard matrix query: %s\nquery for wildcard must be one of: \"=\", nothing\nfor the matrix property: %s\nfor the rows axis: %s\nfor the columns axis: %s\nfor the view: %s\nof the daf data: %s",
                        as.character(q), nns, rrs, ccs, "view!",
                        S7::prop(daf, "name")
                    ), call. = FALSE)
                }
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
    # Julia parity (V5): substitute `__axis__` placeholder with the
    # slot's base axis, matching DataAxesFormats.jl/src/views.jl
    # template-axis-self-reference.
    q <- gsub("__axis__", override$base_axis, override$query, fixed = TRUE)
    # Auto-prefix bare `:` / `.` queries with the view's base axis.
    trimmed <- trimws(q)
    if (grepl("^[:.]", trimmed) && !grepl("^@", trimmed)) {
        return(sprintf("@ %s %s", override$base_axis, q))
    }
    q
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
    # Julia parity (V4 / V6): a bare `:: <m> ...` (matrix-lookup with no
    # leading axes) auto-prefixes the slot's axes. Mirrors the
    # vector auto-prefix in .view_query_for_vector.
    q <- override$query
    trimmed <- trimws(q)
    if (grepl("^::", trimmed) && !grepl("^@", trimmed)) {
        return(sprintf("@ %s @ %s %s",
            override$base_rows, override$base_cols, q))
    }
    q
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
    value <- get_query(daf@base, .view_query_for_scalar(daf, name))
    .cache_group_value(value, MEMORY_DATA)
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
    inner <- format_axis_array(daf@base, base_axis)
    .cache_group_value(inner$value[idx], MEMORY_DATA)
}

# Build a fresh dict over the post-permutation entries; can't reuse the
# base axis's dict because a view may rename / drop / reorder entries.
S7::method(
    format_axis_dict,
    list(ViewDaf, S7::class_character)
) <- function(daf, axis) {
    entries <- format_axis_array(daf, axis)$value
    dict <- new.env(parent = emptyenv(), size = length(entries))
    for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
    dict
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
        .require_vector(daf, axis, name)
    }
    raw <- get_query(daf@base, q_str)
    # Julia parity: a vector slot whose query produces a matrix is
    # rejected on access (DataAxesFormats.jl/src/views.jl
    # `matrix query: ... for the vector: ...`).
    if (is.matrix(raw) || methods::is(raw, "Matrix")) {
        stop(sprintf(
            "matrix query: %s\nfor the vector: %s\nfor the axis: %s\nfor the view: %s\nof the daf data: %s",
            q_str, name, axis, S7::prop(daf, "name"),
            S7::prop(daf@base, "name")
        ), call. = FALSE)
    }
    idx <- daf@view_axis_indices[[axis]]
    .cache_group_value(raw[idx], MEMORY_DATA)
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
        .require_matrix(daf, rows_axis, columns_axis, name, relayout = FALSE)
    }
    raw <- get_query(daf@base, q_str)
    r_idx <- daf@view_axis_indices[[rows_axis]]
    c_idx <- daf@view_axis_indices[[columns_axis]]
    .cache_group_value(raw[r_idx, c_idx, drop = FALSE], MEMORY_DATA)
}
