#' @include classes.R format_api.R handlers.R anndata_facade.R
NULL

# Internal helper: save the currently-registered "inefficient" handler
# so callers can restore it on exit. Falls back to "warn" if none set.
.dafr_save_inefficient_handler <- function() {
    if (exists("inefficient", envir = .dafr_handlers, inherits = FALSE)) {
        get("inefficient", envir = .dafr_handlers, inherits = FALSE)
    } else {
        "warn"
    }
}

#' Load a Muon-style h5ad file into a `memory_daf`.
#'
#' Minimal-viable h5ad reader covering dense `/X`, `/obs` and `/var`
#' column groups with `_index`, flat scalar `/uns` entries, and
#' dense `/layers`. Sparse matrices, categorical/factor columns,
#' `varm` / `obsm` / `obsp` / `varp`, `raw`, and nested `uns` groups
#' are escalated through `unsupported_handler` and skipped.
#'
#' Requires the `hdf5r` package (a Suggests dependency).
#'
#' @param path Path to the `.h5ad` file.
#' @param name Optional name for the returned Daf; defaults to the
#'   file basename without extension.
#' @param mode `hdf5r` open mode. Default `"r"` (read-only).
#' @param unsupported_handler Handler for h5ad features we don't
#'   translate (default [WARN_HANDLER][handler-constants]). Accepts
#'   `ERROR_HANDLER`, `WARN_HANDLER`, `IGNORE_HANDLER`, or a
#'   `function(message)` dispatched via [inefficient_action_handler()].
#' @return A [memory_daf()] populated with the h5ad contents.
#' @examples
#' \dontrun{
#' p <- system.file("extdata", "small_test.h5ad", package = "dafr")
#' d <- h5ad_as_daf(p)
#' is_daf(d)
#' }
#' @seealso [daf_as_h5ad()], [DafAnnData], [as_anndata()]
#' @export
h5ad_as_daf <- function(path, name = NULL, mode = "r",
                        unsupported_handler = WARN_HANDLER) {
    rlang::check_installed("hdf5r", reason = "for `h5ad_as_daf()`")

    if (!file.exists(path)) {
        stop(sprintf("file does not exist: %s", path), call. = FALSE)
    }

    old_handler <- .dafr_save_inefficient_handler()
    inefficient_action_handler(unsupported_handler)
    on.exit(inefficient_action_handler(old_handler), add = TRUE)

    h5 <- hdf5r::H5File$new(path, mode = mode)
    on.exit(h5$close_all(), add = TRUE)

    daf_name <- name %||% tools::file_path_sans_ext(basename(path))
    d <- memory_daf(name = daf_name)

    # --- obs axis ---
    if (!h5$exists("obs")) {
        stop("h5ad file missing /obs group", call. = FALSE)
    }
    obs_group <- h5[["obs"]]
    if (!obs_group$exists("_index")) {
        stop("h5ad /obs missing _index dataset", call. = FALSE)
    }
    obs_names <- obs_group[["_index"]]$read()
    add_axis(d, "obs", as.character(obs_names))

    # --- var axis ---
    if (!h5$exists("var")) {
        stop("h5ad file missing /var group", call. = FALSE)
    }
    var_group <- h5[["var"]]
    if (!var_group$exists("_index")) {
        stop("h5ad /var missing _index dataset", call. = FALSE)
    }
    var_names <- var_group[["_index"]]$read()
    add_axis(d, "var", as.character(var_names))

    # --- /X matrix ---
    if (h5$exists("X")) {
        x_obj <- h5[["X"]]
        enc <- NULL
        if (inherits(x_obj, "H5Group")) {
            # Sparse X is stored as a group (data, indices, indptr + attrs).
            if (x_obj$attr_exists("encoding-type")) {
                enc <- tryCatch(x_obj$attr_open("encoding-type")$read(),
                    error = function(e) NULL)
            }
            emit_action("inefficient",
                sprintf("sparse X with encoding %s not yet supported; skipping",
                    enc %||% "<unknown>"))
        } else {
            if (x_obj$attr_exists("encoding-type")) {
                enc <- tryCatch(x_obj$attr_open("encoding-type")$read(),
                    error = function(e) NULL)
            }
            if (!is.null(enc) && enc %in% c("csr_matrix", "csc_matrix")) {
                emit_action("inefficient",
                    sprintf("sparse X with encoding %s not yet supported; skipping",
                        enc))
            } else {
                X <- x_obj$read()
                if (!is.matrix(X)) {
                    X <- as.matrix(X)
                }
                set_matrix(d, "obs", "var", "UMIs", X)
            }
        }
    }

    # --- /obs columns (excluding _index) ---
    for (col in obs_group$names) {
        if (col == "_index") next
        child <- obs_group[[col]]
        if (inherits(child, "H5Group")) {
            emit_action("inefficient",
                sprintf("nested obs column '%s' (likely categorical) not supported; skipping",
                    col))
            next
        }
        v <- tryCatch(child$read(), error = function(e) NULL)
        if (is.null(v) || is.list(v)) {
            emit_action("inefficient",
                sprintf("unreadable/list-valued obs column '%s' not supported; skipping",
                    col))
            next
        }
        set_vector(d, "obs", col, v)
    }

    # --- /var columns (excluding _index) ---
    for (col in var_group$names) {
        if (col == "_index") next
        child <- var_group[[col]]
        if (inherits(child, "H5Group")) {
            emit_action("inefficient",
                sprintf("nested var column '%s' (likely categorical) not supported; skipping",
                    col))
            next
        }
        v <- tryCatch(child$read(), error = function(e) NULL)
        if (is.null(v) || is.list(v)) {
            emit_action("inefficient",
                sprintf("unreadable/list-valued var column '%s' not supported; skipping",
                    col))
            next
        }
        set_vector(d, "var", col, v)
    }

    # --- /uns (flat scalars only) ---
    if (h5$exists("uns")) {
        uns <- h5[["uns"]]
        for (nm in uns$names) {
            child <- uns[[nm]]
            if (inherits(child, "H5Group")) {
                emit_action("inefficient",
                    sprintf("nested uns group '%s' not supported; skipping", nm))
                next
            }
            v <- tryCatch(child$read(), error = function(e) NULL)
            if (is.null(v)) {
                emit_action("inefficient",
                    sprintf("unreadable uns '%s'; skipping", nm))
                next
            }
            if (length(v) == 1L) {
                set_scalar(d, nm, v)
            } else {
                emit_action("inefficient",
                    sprintf("non-scalar uns '%s' (length %d) not supported; skipping",
                        nm, length(v)))
            }
        }
    }

    # --- /layers (extra dense matrices on (obs, var)) ---
    if (h5$exists("layers")) {
        layers <- h5[["layers"]]
        for (nm in layers$names) {
            child <- layers[[nm]]
            if (!inherits(child, "H5D")) {
                emit_action("inefficient",
                    sprintf("non-dataset layer '%s' not supported; skipping", nm))
                next
            }
            m <- tryCatch(child$read(), error = function(e) NULL)
            if (is.null(m)) {
                emit_action("inefficient",
                    sprintf("unreadable layer '%s'; skipping", nm))
                next
            }
            if (!is.matrix(m)) m <- as.matrix(m)
            set_matrix(d, "obs", "var", nm, m)
        }
    }

    d
}

#' Write a Daf to a Muon-style h5ad file.
#'
#' Inverse of [h5ad_as_daf()]. Writes a minimal-viable h5ad file
#' containing `/X` (dense), per-axis column groups `/obs` and `/var`
#' (each with an `_index`), `/layers` for additional matrices, and
#' `/uns` for scalars.
#'
#' Requires the `hdf5r` package (a Suggests dependency).
#'
#' @param daf A [DafReader].
#' @param path Destination `.h5ad` path.
#' @param obs_axis Axis name mapped to `/obs`. If `NULL`, auto-detected
#'   from `"cell"`, `"metacell"`.
#' @param var_axis Axis name mapped to `/var`. If `NULL`, auto-detected
#'   from `"gene"`.
#' @param x_name Matrix name (on the `obs_axis, var_axis` pair) written
#'   as `/X`. Default `"UMIs"`.
#' @param overwrite If `FALSE` (default), error when `path` already
#'   exists; if `TRUE`, silently replace.
#' @param unsupported_handler Handler for Daf features we cannot
#'   represent in h5ad (e.g. sparse matrices are densified with a
#'   warning). See [inefficient_action_handler()].
#' @return Invisibly, `path`.
#' @examples
#' \dontrun{
#' d <- memory_daf()
#' add_axis(d, "cell", c("c1", "c2"))
#' add_axis(d, "gene", c("g1", "g2"))
#' set_matrix(d, "cell", "gene", "UMIs", matrix(1:4, 2, 2))
#' p <- tempfile(fileext = ".h5ad")
#' daf_as_h5ad(d, p, obs_axis = "cell", var_axis = "gene")
#' }
#' @seealso [h5ad_as_daf()], [DafAnnData], [as_anndata()]
#' @export
daf_as_h5ad <- function(daf, path, obs_axis = NULL, var_axis = NULL,
                        x_name = "UMIs", overwrite = FALSE,
                        unsupported_handler = WARN_HANDLER) {
    rlang::check_installed("hdf5r", reason = "for `daf_as_h5ad()`")

    if (!is_daf(daf)) {
        stop("`daf` must be a DafReader", call. = FALSE)
    }

    # Overwrite guard — before touching hdf5r, per spec §3 decision 8.
    if (file.exists(path)) {
        if (!overwrite) {
            stop(sprintf("file exists: %s; pass overwrite = TRUE to replace", path),
                 call. = FALSE)
        }
        unlink(path)
    }

    old_handler <- .dafr_save_inefficient_handler()
    inefficient_action_handler(unsupported_handler)
    on.exit(inefficient_action_handler(old_handler), add = TRUE)

    obs_axis <- obs_axis %||% .auto_obs_axis(daf)
    var_axis <- var_axis %||% .auto_var_axis(daf)

    h5 <- hdf5r::H5File$new(path, mode = "w")
    on.exit(h5$close_all(), add = TRUE)

    # Root attrs.
    h5$create_attr("encoding-type",
        robj = "anndata",
        space = hdf5r::H5S$new("scalar"))
    h5$create_attr("encoding-version",
        robj = "0.1.0",
        space = hdf5r::H5S$new("scalar"))

    # Axes.
    obs_names <- format_axis_array(daf, obs_axis)
    var_names <- format_axis_array(daf, var_axis)

    # /X
    if (format_has_matrix(daf, obs_axis, var_axis, x_name)) {
        X <- get_matrix(daf, obs_axis, var_axis, x_name)
        if (methods::is(X, "sparseMatrix")) {
            emit_action("inefficient",
                sprintf("densifying sparse matrix '%s' for h5ad write", x_name))
            X <- as.matrix(X)
        }
        h5$create_dataset("X", robj = X)
    } else {
        emit_action("inefficient",
            sprintf("matrix '%s' not on (%s, %s); writing without /X",
                x_name, obs_axis, var_axis))
    }

    # /obs
    obs_grp <- h5$create_group("obs")
    obs_grp$create_dataset("_index", robj = obs_names)
    for (vn in format_vectors_set(daf, obs_axis)) {
        obs_grp$create_dataset(vn, robj = format_get_vector(daf, obs_axis, vn))
    }

    # /var
    var_grp <- h5$create_group("var")
    var_grp$create_dataset("_index", robj = var_names)
    for (vn in format_vectors_set(daf, var_axis)) {
        var_grp$create_dataset(vn, robj = format_get_vector(daf, var_axis, vn))
    }

    # /layers — matrices on (obs_axis, var_axis) other than x_name.
    layers_grp <- h5$create_group("layers")
    for (nm in format_matrices_set(daf, obs_axis, var_axis)) {
        if (nm == x_name) next
        m <- get_matrix(daf, obs_axis, var_axis, nm)
        if (methods::is(m, "sparseMatrix")) {
            emit_action("inefficient",
                sprintf("densifying sparse matrix '%s' for h5ad write", nm))
            m <- as.matrix(m)
        }
        layers_grp$create_dataset(nm, robj = m)
    }

    # /uns — flat scalars.
    uns_grp <- h5$create_group("uns")
    for (nm in format_scalars_set(daf)) {
        v <- get_scalar(daf, nm)
        uns_grp$create_dataset(nm, robj = v)
    }

    invisible(path)
}
