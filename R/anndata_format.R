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

# ---- Sparse helpers -------------------------------------------------------

# Read an h5ad sparse matrix group (csr_matrix or csc_matrix) into a
# dgCMatrix with shape (n_rows, n_cols) as declared in the `shape` attr.
.read_h5ad_sparse <- function(grp) {
    enc <- hdf5r::h5attr(grp, "encoding-type")
    shape <- hdf5r::h5attr(grp, "shape")
    data <- grp[["data"]]$read()
    indices <- grp[["indices"]]$read()
    indptr <- grp[["indptr"]]$read()
    nrow <- as.integer(shape[1L])
    ncol <- as.integer(shape[2L])
    if (enc == "csr_matrix") {
        # CSR: indptr is over rows (length = nrow + 1), indices are
        # column indices (0-based). Build a dgRMatrix then coerce.
        M <- methods::new("dgRMatrix",
            Dim = c(nrow, ncol),
            p = as.integer(indptr),
            j = as.integer(indices),
            x = as.numeric(data)
        )
        methods::as(M, "CsparseMatrix")
    } else if (enc == "csc_matrix") {
        # CSC: indptr is over columns, indices are row indices (0-based).
        Matrix::sparseMatrix(
            i = as.integer(indices) + 1L,
            p = as.integer(indptr),
            x = as.numeric(data),
            dims = c(nrow, ncol),
            repr = "C"
        )
    } else {
        stop(sprintf("unsupported sparse encoding: %s", enc), call. = FALSE)
    }
}

# Write a sparseMatrix as an h5ad CSC group under `parent/name`.
.write_h5ad_sparse <- function(parent, name, m) {
    if (!methods::is(m, "CsparseMatrix")) {
        m <- methods::as(m, "CsparseMatrix")
    }
    grp <- parent$create_group(name)
    grp$create_attr("encoding-type", robj = "csc_matrix",
                    space = hdf5r::H5S$new("scalar"))
    grp$create_attr("encoding-version", robj = "0.1.0",
                    space = hdf5r::H5S$new("scalar"))
    grp$create_attr("shape", robj = as.integer(dim(m)))
    grp$create_dataset("data", robj = as.numeric(m@x))
    grp$create_dataset("indices", robj = as.integer(m@i))  # 0-based row idx
    grp$create_dataset("indptr", robj = as.integer(m@p))   # col pointers
    invisible(NULL)
}

# ---- Categorical helpers --------------------------------------------------

# Read a categorical obs/var column group. Returns a factor (ordered or
# unordered depending on the `ordered` attr). NA codes (-1) become NA.
.read_h5ad_categorical <- function(grp) {
    codes <- grp[["codes"]]$read()
    categories <- grp[["categories"]]$read()
    ordered <- FALSE
    if (grp$attr_exists("ordered")) {
        ordered <- tryCatch(isTRUE(as.logical(hdf5r::h5attr(grp, "ordered"))[[1L]]),
            error = function(e) FALSE)
    }
    idx <- as.integer(codes) + 1L
    idx[as.integer(codes) < 0L] <- NA_integer_
    structure(idx,
        levels = as.character(categories),
        class = if (ordered) c("ordered", "factor") else "factor"
    )
}

# Write a factor vector as a categorical group under `parent/name`.
.write_h5ad_categorical <- function(parent, name, v) {
    stopifnot(is.factor(v))
    grp <- parent$create_group(name)
    grp$create_attr("encoding-type", robj = "categorical",
                    space = hdf5r::H5S$new("scalar"))
    grp$create_attr("encoding-version", robj = "0.2.0",
                    space = hdf5r::H5S$new("scalar"))
    grp$create_attr("ordered", robj = is.ordered(v))
    codes <- as.integer(v) - 1L  # 0-based
    codes[is.na(codes)] <- -1L
    grp$create_dataset("codes", robj = as.integer(codes))
    grp$create_dataset("categories", robj = as.character(levels(v)))
    invisible(NULL)
}

# ---- Nested uns helper ----------------------------------------------------

# Walk a /uns group recursively. Each nested scalar leaf gets flattened
# into the parent name-space via a `_` separator. Non-scalar leaves are
# skipped (same policy as the flat reader).
.flatten_uns <- function(grp, prefix = "") {
    out <- list()
    for (nm in grp$names) {
        child <- grp[[nm]]
        full_name <- if (nzchar(prefix)) paste0(prefix, "_", nm) else nm
        if (inherits(child, "H5Group")) {
            out <- c(out, .flatten_uns(child, full_name))
        } else {
            v <- tryCatch(child$read(), error = function(e) NULL)
            if (!is.null(v) && length(v) == 1L) {
                out[[full_name]] <- v
            }
            # Non-scalar or unreadable uns entries: skipped silently here;
            # the top-level caller is responsible for any warnings.
        }
    }
    out
}

#' Load a Muon-style h5ad file into a `memory_daf`.
#'
#' Loads dense or sparse `/X`, `/obs` and `/var` column groups (dense
#' scalar columns and categorical columns), flat + nested `/uns` scalar
#' entries (nested keys are flattened via `_`), dense `/layers`, and
#' `/obsm` / `/varm` dense matrices (each stored on a synthetic axis
#' `obsm_<name>_dim` / `varm_<name>_dim`). `obsp` / `varp` and `raw` are
#' still escalated through `unsupported_handler` and skipped.
#'
#' Requires the `hdf5r` and `Matrix` packages.
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
        if (inherits(x_obj, "H5Group")) {
            # Sparse X: detect encoding and read via helper.
            enc <- NULL
            if (x_obj$attr_exists("encoding-type")) {
                enc <- tryCatch(hdf5r::h5attr(x_obj, "encoding-type"),
                    error = function(e) NULL)
            }
            if (!is.null(enc) && enc %in% c("csr_matrix", "csc_matrix")) {
                X <- .read_h5ad_sparse(x_obj)
                set_matrix(d, "obs", "var", "UMIs", X)
            } else {
                emit_action("inefficient",
                    sprintf("group-valued X with encoding %s not supported; skipping",
                        enc %||% "<unknown>"))
            }
        } else {
            X <- x_obj$read()
            if (!is.matrix(X)) {
                X <- as.matrix(X)
            }
            set_matrix(d, "obs", "var", "UMIs", X)
        }
    }

    # --- /obs columns (excluding _index) ---
    for (col in obs_group$names) {
        if (col == "_index") next
        child <- obs_group[[col]]
        if (inherits(child, "H5Group")) {
            enc <- if (child$attr_exists("encoding-type")) {
                tryCatch(hdf5r::h5attr(child, "encoding-type"),
                    error = function(e) NULL)
            } else {
                NULL
            }
            if (!is.null(enc) && enc == "categorical") {
                v <- .read_h5ad_categorical(child)
                set_vector(d, "obs", col, v)
                next
            }
            emit_action("inefficient",
                sprintf("nested obs column '%s' (encoding=%s) not supported; skipping",
                    col, enc %||% "unknown"))
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
            enc <- if (child$attr_exists("encoding-type")) {
                tryCatch(hdf5r::h5attr(child, "encoding-type"),
                    error = function(e) NULL)
            } else {
                NULL
            }
            if (!is.null(enc) && enc == "categorical") {
                v <- .read_h5ad_categorical(child)
                set_vector(d, "var", col, v)
                next
            }
            emit_action("inefficient",
                sprintf("nested var column '%s' (encoding=%s) not supported; skipping",
                    col, enc %||% "unknown"))
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

    # --- /uns (flat + nested scalars, flattened via '_' separator) ---
    if (h5$exists("uns")) {
        uns <- h5[["uns"]]
        flat <- .flatten_uns(uns)
        for (nm in names(flat)) {
            set_scalar(d, nm, flat[[nm]])
        }
    }

    # --- /layers (extra dense matrices on (obs, var)) ---
    if (h5$exists("layers")) {
        layers <- h5[["layers"]]
        for (nm in layers$names) {
            child <- layers[[nm]]
            if (inherits(child, "H5Group")) {
                # Sparse layer — same shape convention as X.
                enc <- if (child$attr_exists("encoding-type")) {
                    tryCatch(hdf5r::h5attr(child, "encoding-type"),
                        error = function(e) NULL)
                } else {
                    NULL
                }
                if (!is.null(enc) && enc %in% c("csr_matrix", "csc_matrix")) {
                    m <- .read_h5ad_sparse(child)
                    set_matrix(d, "obs", "var", nm, m)
                    next
                }
                emit_action("inefficient",
                    sprintf("non-dataset layer '%s' (encoding=%s) not supported; skipping",
                        nm, enc %||% "unknown"))
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

    # --- /obsm and /varm — per-axis embeddings / loadings.
    # Each dense (n_axis x k) matrix is stored as a daf matrix on a
    # synthetic axis `<obsm|varm>_<name>_dim` with entries "1".."k". The
    # matrix name equals the embedding name.
    .read_xm <- function(group_name, axis) {
        if (!h5$exists(group_name)) return(invisible())
        grp <- h5[[group_name]]
        for (nm in grp$names) {
            child <- grp[[nm]]
            if (inherits(child, "H5Group")) {
                emit_action("inefficient",
                    sprintf("non-dataset %s entry '%s' not supported; skipping",
                        group_name, nm))
                next
            }
            m <- tryCatch(child$read(), error = function(e) NULL)
            if (is.null(m)) {
                emit_action("inefficient",
                    sprintf("unreadable %s entry '%s'; skipping", group_name, nm))
                next
            }
            if (!is.matrix(m)) {
                emit_action("inefficient",
                    sprintf("non-matrix %s entry '%s' not supported; skipping",
                        group_name, nm))
                next
            }
            synth <- sprintf("%s_%s_dim", group_name, nm)
            add_axis(d, synth, as.character(seq_len(ncol(m))))
            set_matrix(d, axis, synth, nm, m)
        }
    }
    .read_xm("obsm", "obs")
    .read_xm("varm", "var")

    d
}

#' Write a Daf to a Muon-style h5ad file.
#'
#' Inverse of [h5ad_as_daf()]. Writes `/X` (dense or CSC-sparse),
#' per-axis column groups `/obs` and `/var` (each with an `_index`;
#' factor vectors emitted as h5ad categorical groups), `/layers` for
#' additional matrices, and flat `/uns` scalars.
#'
#' Note: the write side always emits `/uns` flat. On read we flatten
#' nested uns groups with a `_` separator, so a round-trip of a nested
#' uns file produces flat dotted keys on the resulting Daf.
#'
#' Requires the `hdf5r` and `Matrix` packages.
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
#'   represent in h5ad. See [inefficient_action_handler()].
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
            .write_h5ad_sparse(h5, "X", X)
        } else {
            h5$create_dataset("X", robj = X)
        }
    } else {
        emit_action("inefficient",
            sprintf("matrix '%s' not on (%s, %s); writing without /X",
                x_name, obs_axis, var_axis))
    }

    # /obs
    obs_grp <- h5$create_group("obs")
    obs_grp$create_dataset("_index", robj = obs_names)
    for (vn in format_vectors_set(daf, obs_axis)) {
        v <- format_get_vector(daf, obs_axis, vn)
        if (is.factor(v)) {
            .write_h5ad_categorical(obs_grp, vn, v)
        } else {
            obs_grp$create_dataset(vn, robj = v)
        }
    }

    # /var
    var_grp <- h5$create_group("var")
    var_grp$create_dataset("_index", robj = var_names)
    for (vn in format_vectors_set(daf, var_axis)) {
        v <- format_get_vector(daf, var_axis, vn)
        if (is.factor(v)) {
            .write_h5ad_categorical(var_grp, vn, v)
        } else {
            var_grp$create_dataset(vn, robj = v)
        }
    }

    # /layers — matrices on (obs_axis, var_axis) other than x_name.
    layers_grp <- h5$create_group("layers")
    for (nm in format_matrices_set(daf, obs_axis, var_axis)) {
        if (nm == x_name) next
        m <- get_matrix(daf, obs_axis, var_axis, nm)
        if (methods::is(m, "sparseMatrix")) {
            .write_h5ad_sparse(layers_grp, nm, m)
        } else {
            layers_grp$create_dataset(nm, robj = m)
        }
    }

    # /obsm and /varm — matrices living on (<axis>, <synthetic dim axis>),
    # where the dim axis is named `<obsm|varm>_<matrix_name>_dim`. This is
    # the inverse of the synthetic-axis convention used by h5ad_as_daf.
    .write_xm <- function(group_prefix, axis) {
        grp <- h5$create_group(group_prefix)
        # Any axis matching the naming convention is a candidate.
        all_axes <- format_axes_set(daf)
        pat <- sprintf("^%s_(.+)_dim$", group_prefix)
        for (ax in all_axes) {
            mm <- regmatches(ax, regexec(pat, ax))[[1L]]
            if (length(mm) != 2L) next
            synth_axis <- ax
            # Enumerate matrices on (axis, synth_axis); any matrix there is
            # an embedding for that synthetic axis.
            for (mname in format_matrices_set(daf, axis, synth_axis)) {
                m <- get_matrix(daf, axis, synth_axis, mname)
                if (methods::is(m, "sparseMatrix")) {
                    m <- as.matrix(m)
                }
                # Shape must be (n_axis, k) — matches the synthetic axis on
                # the right. If the matrix comes back flipped, restore the
                # canonical orientation.
                if (nrow(m) != format_axis_length(daf, axis)) {
                    m <- t(m)
                }
                grp$create_dataset(mname, robj = m)
            }
        }
    }
    .write_xm("obsm", obs_axis)
    .write_xm("varm", var_axis)

    # /uns — flat scalars (no nested write).
    uns_grp <- h5$create_group("uns")
    for (nm in format_scalars_set(daf)) {
        v <- get_scalar(daf, nm)
        uns_grp$create_dataset(nm, robj = v)
    }

    invisible(path)
}
