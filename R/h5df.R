#' @include classes.R format_api.R files_io.R utils.R cache.R
NULL

# H5df: a whole Daf store in one .h5df HDF5 file, interoperable with
# DataAxesFormats.jl H5df. Layout (no attributes): a `daf` dataset holding
# UInt8[1,0] marks the store; scalars/axes/vectors/matrices groups hold typed
# HDF5 datasets. Sparsity = group-vs-dataset. Built on hdf5r (Suggests).

#' File-backed (HDF5) Daf writer class.
#'
#' Concrete `DafWriter` subclass instantiated by [h5df()] for writable modes
#' (`"r+"`, `"w"`, `"w+"`). Use [h5df()] to construct instances.
#' @inheritParams DafReader
#' @export
H5df <- S7::new_class(name = "H5df", package = "dafr", parent = DafWriter)

#' File-backed (HDF5) read-only Daf class.
#'
#' Concrete `DafReadOnly` subclass instantiated by [h5df()] with mode `"r"`.
#' @inheritParams DafReader
#' @export
H5dfReadOnly <- S7::new_class(
    name = "H5dfReadOnly", package = "dafr", parent = DafReadOnly
)

# ---- handle & key helpers ----
# Grouped .h5dfs#/group stores are deferred, so the H5File IS the store root.
.h5_root <- function(daf) S7::prop(daf, "internal")$h5
.hkey_scalar <- function(name) paste0("scalars/", name)
.hkey_axis   <- function(axis) paste0("axes/", axis)
.hkey_vector <- function(axis, name) paste0("vectors/", axis, "/", name)
.hkey_matrix <- function(ra, ca, name) paste0("matrices/", ra, "/", ca, "/", name)

.h5_read_only_guard <- function(verb) {
    stop(sprintf("h5df: store opened read-only; %s not permitted", verb),
        call. = FALSE)
}

# dafr mode -> hdf5r H5File mode
.H5_OPEN_MODE <- c(r = "r", "r+" = "r+", w = "w", "w+" = "a")

# indtype name ("UInt16"/...) -> hdf5r native unsigned type object
.h5_uint_type <- function(indtype) {
    switch(indtype,
        UInt16 = hdf5r::h5types$H5T_NATIVE_UINT16,
        UInt32 = hdf5r::h5types$H5T_NATIVE_UINT32,
        UInt64 = hdf5r::h5types$H5T_NATIVE_UINT64,
        stop(sprintf("h5df: unexpected index type %s", indtype), call. = FALSE))
}

.h5_write_index <- function(grp, name, values, indtype) {
    grp$create_dataset(name, robj = as.integer(values),
        dtype = .h5_uint_type(indtype), chunk_dims = NULL)
    invisible()
}

.h5_check_version <- function(h5, path) {
    v <- as.integer(h5[["daf"]]$read())
    if (length(v) != 2L || v[[1L]] != 1L || v[[2L]] > 0L) {
        stop(sprintf(paste0(
            "incompatible format version: %s\nfor the h5df: %s\n",
            "the code supports version: 1.0"),
            paste(v, collapse = "."), path), call. = FALSE)
    }
    invisible()
}

# ==== constructor ============================================================

#' Single-file (HDF5) Daf store.
#'
#' A `Daf` store held in one `.h5df` HDF5 file, interoperable with Julia's
#' `DataAxesFormats.H5df`. The file holds a `daf` marker dataset plus
#' `scalars`/`axes`/`vectors`/`matrices` groups of typed HDF5 datasets.
#' Requires the `hdf5r` package.
#'
#' @param path Path to a `.h5df` file.
#' @param mode One of `"r"` (read; must exist), `"r+"` (append; must exist),
#'   `"w"` (create; fails if it is already a daf store), `"w+"` (create or
#'   append).
#' @param name Human-readable identifier. Default derived from the store's
#'   `name` scalar if present, else `basename(path)`.
#' @return An `H5df` (writable modes) or `H5dfReadOnly` (`"r"`).
#' @examples
#' if (requireNamespace("hdf5r", quietly = TRUE)) {
#'   path <- tempfile("dafr-", fileext = ".h5df")
#'   d <- h5df(path, mode = "w")
#'   add_axis(d, "cell", c("c1", "c2"))
#'   set_scalar(d, "organism", "human")
#'   rm(d)
#'   unlink(path)
#' }
#' @export
h5df <- function(path, mode = c("r", "r+", "w", "w+"), name = NULL) {
    stopifnot(is.character(path), length(path) == 1L, !is.na(path))
    rlang::check_installed("hdf5r", reason = "for `h5df()`")
    mode <- match.arg(mode)
    if (mode == "w" && file.exists(path)) {
        already <- tryCatch({
            h0 <- hdf5r::H5File$new(path, mode = "r")
            res <- h0$exists("daf")
            h0$close_all()
            res
        }, error = function(e) FALSE)
        if (isTRUE(already)) {
            stop(sprintf("h5df(%s, 'w'): file is already a daf store; use 'w+'",
                sQuote(path)), call. = FALSE)
        }
    }
    if (mode %in% c("r", "r+") && !file.exists(path)) {
        stop(sprintf("h5df(%s, '%s'): not a daf store (file does not exist)",
            sQuote(path), mode), call. = FALSE)
    }
    h5 <- hdf5r::H5File$new(path, mode = unname(.H5_OPEN_MODE[[mode]]))
    has_marker <- h5$exists("daf")
    if (mode %in% c("r", "r+") && !has_marker) {
        stop(sprintf("h5df(%s, '%s'): not a daf store (no daf marker)",
            sQuote(path), mode), call. = FALSE)
    }
    if (mode %in% c("w", "w+") && !has_marker) {
        for (g in c("scalars", "axes", "vectors", "matrices")) h5$create_group(g)
        h5$create_dataset("daf", robj = c(1L, 0L),
            dtype = hdf5r::h5types$H5T_NATIVE_UINT8, chunk_dims = NULL)
    }
    .h5_check_version(h5, path)
    if (is.null(name)) {
        name <- if (h5$exists(.hkey_scalar("name"))) {
            as.character(h5[[.hkey_scalar("name")]]$read())
        } else {
            basename(path)
        }
    }
    .assert_name(name, "name")
    internal <- new_internal_env()
    internal$h5 <- h5
    internal$path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    internal$mode <- mode
    internal$axes <- new.env(parent = emptyenv())
    ctor <- if (mode == "r") H5dfReadOnly else H5df
    ctor(
        name = name, internal = internal, cache = new_cache_env(),
        axis_version_counter = new_counter_env(),
        vector_version_counter = new_counter_env(),
        matrix_version_counter = new_counter_env()
    )
}

# ==== S7 registrations: reader-side (both writer + read-only) ================
# (extended by later tasks; scalars/axes/vectors/matrices readers added later)
local({
    for (cls in list(H5df, H5dfReadOnly)) {
        S7::method(.is_leaf_dispatch, cls) <- function(daf) TRUE
        S7::method(format_description_header, cls) <-
            function(daf, indent = "", deep = FALSE) {
                internal <- S7::prop(daf, "internal")
                c(paste0(indent, "type: H5df"),
                  paste0(indent, "path: ", internal$path),
                  paste0(indent, "mode: ", internal$mode))
            }
    }
})

# ==== scalars ================================================================

.h5_has_scalar <- function(daf, name) .h5_root(daf)$exists(.hkey_scalar(name))
.h5_get_scalar <- function(daf, name) {
    root <- .h5_root(daf); key <- .hkey_scalar(name)
    if (!root$exists(key)) .require_scalar(daf, name)
    root[[key]]$read()
}
.h5_scalars_set <- function(daf) {
    root <- .h5_root(daf)
    if (!root$exists("scalars")) return(character(0L))
    sort(root[["scalars"]]$names, method = "radix")
}
.h5_set_scalar <- function(daf, name, value, overwrite) {
    .assert_scalar_value(name, value)
    root <- .h5_root(daf); key <- .hkey_scalar(name)
    if (!overwrite) .require_no_scalar(daf, name)
    if (root$exists(key)) root$link_delete(key)
    root$create_dataset(key, robj = value,
        space = hdf5r::H5S$new("scalar"), chunk_dims = NULL)
    MEMORY_DATA
}
.h5_delete_scalar <- function(daf, name, must_exist) {
    root <- .h5_root(daf); key <- .hkey_scalar(name)
    if (!root$exists(key)) {
        if (must_exist) .require_scalar(daf, name)
        return(invisible())
    }
    root$link_delete(key)
    invisible()
}

local({
    for (cls in list(H5df, H5dfReadOnly)) {
        S7::method(format_has_scalar, list(cls, S7::class_character)) <-
            function(daf, name) .h5_has_scalar(daf, name)
        S7::method(format_get_scalar, list(cls, S7::class_character)) <-
            function(daf, name) .cache_group_value(.h5_get_scalar(daf, name), MEMORY_DATA)
        S7::method(format_scalars_set, cls) <- function(daf) .h5_scalars_set(daf)
    }
})

S7::method(format_set_scalar,
    list(H5df, S7::class_character, S7::class_any, S7::class_logical)) <-
    function(daf, name, value, overwrite) .h5_set_scalar(daf, name, value, overwrite)
S7::method(format_delete_scalar, list(H5df, S7::class_character, S7::class_logical)) <-
    function(daf, name, must_exist) .h5_delete_scalar(daf, name, must_exist)

# hdf5r crashes reclaiming an empty vlen-string buffer; return a typed empty
# vector for zero-length datasets, otherwise read normally.
.h5_safe_read <- function(obj) {
    if (prod(obj$dims) == 0L) {
        return(tryCatch(obj$read(), error = function(e) character(0L)))
    }
    obj$read()
}

# ==== axes ===================================================================

.h5_axis_parsed <- function(daf, axis) {
    cache <- S7::prop(daf, "internal")$axes
    if (exists(axis, envir = cache, inherits = FALSE)) {
        return(get(axis, envir = cache, inherits = FALSE))
    }
    root <- .h5_root(daf); key <- .hkey_axis(axis)
    if (!root$exists(key)) return(NULL)
    entries <- as.character(.h5_safe_read(root[[key]]))
    if (anyNA(entries) || (length(entries) && any(!nzchar(entries)))) {
        stop(sprintf("h5df: axis %s contains empty entries", sQuote(axis)), call. = FALSE)
    }
    dict <- new.env(parent = emptyenv(), size = length(entries))
    for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
    parsed <- list(entries = entries, dict = dict)
    assign(axis, parsed, envir = cache)
    parsed
}
.h5_axis_require <- function(daf, axis) {
    parsed <- .h5_axis_parsed(daf, axis)
    if (is.null(parsed)) .require_axis(daf, "for: h5df backend", axis)
    parsed
}
.h5_has_axis <- function(daf, axis) .h5_root(daf)$exists(.hkey_axis(axis))
.h5_axes_set <- function(daf) {
    root <- .h5_root(daf)
    if (!root$exists("axes")) return(character(0L))
    sort(root[["axes"]]$names, method = "radix")
}

.h5_add_axis <- function(daf, axis, entries) {
    if (!is.character(entries)) {
        stop(sprintf("axis %s entries must be a character vector", sQuote(axis)), call. = FALSE)
    }
    if (anyNA(entries)) {
        stop(sprintf("axis %s entries contain NA", sQuote(axis)), call. = FALSE)
    }
    if (any(!nzchar(entries))) {
        stop(sprintf("axis %s entries contain empty strings", sQuote(axis)), call. = FALSE)
    }
    if (any(grepl("[\n\r]", entries))) {
        stop(sprintf("axis %s entries contain newline characters", sQuote(axis)), call. = FALSE)
    }
    if (anyDuplicated(entries)) {
        stop(sprintf("non-unique entries for new axis: %s\nin the daf data: %s",
            axis, S7::prop(daf, "name")), call. = FALSE)
    }
    .require_no_axis(daf, axis)
    root <- .h5_root(daf)
    # A zero-length `robj` writes a valid empty vlen-string dataset; reads of it
    # go through `.h5_safe_read` (hdf5r crashes reading empty vlen strings).
    root$create_dataset(.hkey_axis(axis), robj = entries, chunk_dims = NULL)
    # Eagerly create vectors/<axis> and every matrices/<a>/<b> pairing (incl.
    # self) so a Julia reader scanning the store does not trip on missing groups.
    root$create_group(paste0("vectors/", axis))
    existing <- root[["matrices"]]$names
    root$create_group(paste0("matrices/", axis))
    for (other in existing) {
        root$create_group(paste0("matrices/", axis, "/", other))
        root$create_group(paste0("matrices/", other, "/", axis))
    }
    root$create_group(paste0("matrices/", axis, "/", axis))
    dict <- new.env(parent = emptyenv(), size = length(entries))
    for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
    assign(axis, list(entries = entries, dict = dict),
        envir = S7::prop(daf, "internal")$axes)
    invisible()
}

.h5_delete_axis <- function(daf, axis, must_exist) {
    root <- .h5_root(daf); key <- .hkey_axis(axis)
    if (!root$exists(key)) {
        if (must_exist) .require_axis(daf, "for: delete_axis", axis)
        return(invisible())
    }
    root$link_delete(key)
    if (root$exists(paste0("vectors/", axis))) root$link_delete(paste0("vectors/", axis))
    if (root$exists(paste0("matrices/", axis))) root$link_delete(paste0("matrices/", axis))
    for (other in root[["matrices"]]$names) {
        k <- paste0("matrices/", other, "/", axis)
        if (root$exists(k)) root$link_delete(k)
    }
    cache <- S7::prop(daf, "internal")$axes
    if (exists(axis, envir = cache, inherits = FALSE)) rm(list = axis, envir = cache)
    bump_axis_counter(daf, axis)
    invisible()
}

local({
    for (cls in list(H5df, H5dfReadOnly)) {
        S7::method(format_has_axis, list(cls, S7::class_character)) <-
            function(daf, axis) .h5_has_axis(daf, axis)
        S7::method(format_axes_set, cls) <- function(daf) .h5_axes_set(daf)
        S7::method(format_axis_length, list(cls, S7::class_character)) <-
            function(daf, axis) length(.h5_axis_require(daf, axis)$entries)
        S7::method(format_axis_array, list(cls, S7::class_character)) <-
            function(daf, axis) .cache_group_value(.h5_axis_require(daf, axis)$entries, MEMORY_DATA)
        S7::method(format_axis_dict, list(cls, S7::class_character)) <-
            function(daf, axis) .h5_axis_require(daf, axis)$dict
    }
})

S7::method(format_add_axis, list(H5df, S7::class_character, S7::class_character)) <-
    function(daf, axis, entries) .h5_add_axis(daf, axis, entries)
S7::method(format_delete_axis, list(H5df, S7::class_character, S7::class_logical)) <-
    function(daf, axis, must_exist) .h5_delete_axis(daf, axis, must_exist)

# ==== vectors ================================================================

.h5_has_vector <- function(daf, axis, name) {
    if (!format_has_axis(daf, axis)) return(FALSE)
    .h5_root(daf)$exists(.hkey_vector(axis, name))
}
.h5_vectors_set <- function(daf, axis) {
    if (!format_has_axis(daf, axis)) return(character(0L))
    root <- .h5_root(daf); key <- paste0("vectors/", axis)
    if (!root$exists(key)) return(character(0L))
    sort(root[[key]]$names, method = "radix")
}

# Scatter a sparse-group vector to a dense R vector (H5df returns dense
# vectors, matching ZipDaf).
.h5_read_sparse_vector <- function(grp, n) {
    idx <- as.integer(grp[["nzind"]]$read())        # 1-based
    if (grp$exists("nztxt")) {
        vals <- .h5_safe_read(grp[["nztxt"]])
        out <- rep("", n); out[idx] <- as.character(vals); return(out)
    }
    if (grp$exists("nzval")) {
        vals <- grp[["nzval"]]$read()
        out <- if (is.logical(vals)) logical(n) else vector(typeof(vals), n)
        out[idx] <- vals; return(out)
    }
    out <- logical(n); out[idx] <- TRUE; out        # bool-all-true: nzval omitted
}

.h5_get_vector_impl <- function(daf, axis, name) {
    root <- .h5_root(daf); key <- .hkey_vector(axis, name)
    if (!root$exists(key)) .require_vector(daf, axis, name)
    n <- format_axis_length(daf, axis)
    obj <- root[[key]]
    if (inherits(obj, "H5Group")) return(.h5_read_sparse_vector(obj, n))
    .h5_safe_read(obj)
}

.h5_set_vector_sparse <- function(daf, axis, name, sv, overwrite) {
    n <- format_axis_length(daf, axis)
    if (sv@length != n) {
        stop(sprintf("sparseVector %s length %d (expected %d) on axis %s",
            sQuote(name), sv@length, n, sQuote(axis)), call. = FALSE)
    }
    if (!overwrite) .require_no_vector(daf, axis, name)
    root <- .h5_root(daf); key <- .hkey_vector(axis, name)
    if (root$exists(key)) root$link_delete(key)
    grp <- root$create_group(key)
    .h5_write_index(grp, "nzind", as.integer(sv@i), .indtype_for_size(n))  # @i is 1-based
    eltype <- .dtype_for_r_vector(sv@x)
    if (eltype == "Bool") {
        if (!all(sv@x)) grp$create_dataset("nzval", robj = as.logical(sv@x), chunk_dims = NULL)
    } else {
        grp$create_dataset("nzval", robj = sv@x, chunk_dims = NULL)
    }
    bump_vector_counter(daf, axis, name)
    MEMORY_DATA
}

# H5df stores dense input as a dense dataset and sparseVector input as a sparse
# group; it does NOT auto-sparsify dense input.
# ponytail: no sparsify heuristic; add one only if store size becomes a problem.
.h5_set_vector <- function(daf, axis, name, vec, overwrite) {
    if (methods::is(vec, "sparseVector")) {
        return(.h5_set_vector_sparse(daf, axis, name, vec, overwrite))
    }
    vec <- .validate_vector_value(daf, axis, name, vec)
    if (!overwrite) .require_no_vector(daf, axis, name)
    root <- .h5_root(daf); key <- .hkey_vector(axis, name)
    if (root$exists(key)) root$link_delete(key)
    # Empty `robj` is fine (writes a valid empty dataset); reads use `.h5_safe_read`.
    root$create_dataset(key, robj = vec, chunk_dims = NULL)
    bump_vector_counter(daf, axis, name)
    MEMORY_DATA
}

.h5_delete_vector <- function(daf, axis, name, must_exist) {
    root <- .h5_root(daf); key <- .hkey_vector(axis, name)
    if (!root$exists(key)) {
        if (must_exist) .require_vector(daf, axis, name)
        return(invisible())
    }
    root$link_delete(key)
    invisible()
}

local({
    for (cls in list(H5df, H5dfReadOnly)) {
        S7::method(format_has_vector, list(cls, S7::class_character, S7::class_character)) <-
            function(daf, axis, name) .h5_has_vector(daf, axis, name)
        S7::method(format_vectors_set, list(cls, S7::class_character)) <-
            function(daf, axis) .h5_vectors_set(daf, axis)
        S7::method(format_get_vector, list(cls, S7::class_character, S7::class_character)) <-
            function(daf, axis, name) {
                v <- .h5_get_vector_impl(daf, axis, name)
                .cache_group_value(.attach_vector_axis_names(daf, axis, v),
                    .files_daf_classify_vector(v))
            }
    }
})

S7::method(format_set_vector,
    list(H5df, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)) <-
    function(daf, axis, name, vec, overwrite) .h5_set_vector(daf, axis, name, vec, overwrite)
S7::method(format_delete_vector,
    list(H5df, S7::class_character, S7::class_character, S7::class_logical)) <-
    function(daf, axis, name, must_exist) .h5_delete_vector(daf, axis, name, must_exist)

# ==== matrices ===============================================================

.h5_has_matrix <- function(daf, ra, ca, name) {
    if (!format_has_axis(daf, ra) || !format_has_axis(daf, ca)) return(FALSE)
    .h5_root(daf)$exists(.hkey_matrix(ra, ca, name))
}
.h5_matrices_set <- function(daf, ra, ca) {
    if (!format_has_axis(daf, ra) || !format_has_axis(daf, ca)) return(character(0L))
    root <- .h5_root(daf); key <- paste0("matrices/", ra, "/", ca)
    if (!root$exists(key)) return(character(0L))
    sort(root[[key]]$names, method = "radix")
}

.h5_read_sparse_matrix <- function(grp, nr, nc) {
    colptr <- as.integer(grp[["colptr"]]$read())
    rowval <- as.integer(grp[["rowval"]]$read())
    if (grp$exists("nztxt")) {                        # Julia-written string-sparse
        txt <- as.character(.h5_safe_read(grp[["nztxt"]]))
        m <- matrix("", nr, nc)
        for (j in seq_len(nc)) {
            if (colptr[j + 1L] > colptr[j]) {
                rng <- colptr[j]:(colptr[j + 1L] - 1L)
                m[rowval[rng], j] <- txt[rng]
            }
        }
        return(m)
    }
    i0 <- as.integer(rowval) - 1L
    p0 <- as.integer(colptr) - 1L
    if (grp$exists("nzval")) {
        vals <- grp[["nzval"]]$read()
        if (is.logical(vals)) {
            return(methods::new("lgCMatrix", x = vals, i = i0, p = p0,
                Dim = c(as.integer(nr), as.integer(nc)), Dimnames = list(NULL, NULL)))
        }
        return(methods::new("dgCMatrix", x = as.double(vals), i = i0, p = p0,
            Dim = c(as.integer(nr), as.integer(nc)), Dimnames = list(NULL, NULL)))
    }
    methods::new("lgCMatrix", x = rep(TRUE, length(i0)), i = i0, p = p0,   # bool-all-true
        Dim = c(as.integer(nr), as.integer(nc)), Dimnames = list(NULL, NULL))
}

.h5_get_matrix_impl <- function(daf, ra, ca, name) {
    root <- .h5_root(daf); key <- .hkey_matrix(ra, ca, name)
    if (!root$exists(key)) .require_matrix(daf, ra, ca, name, relayout = FALSE)
    nr <- format_axis_length(daf, ra); nc <- format_axis_length(daf, ca)
    obj <- root[[key]]
    if (inherits(obj, "H5Group")) return(.h5_read_sparse_matrix(obj, nr, nc))
    v <- .h5_safe_read(obj)                            # hdf5r reverses dims -> (nr, nc)
    if (is.null(dim(v))) dim(v) <- c(as.integer(nr), as.integer(nc))
    v
}

.h5_write_matrix_sparse <- function(root, key, mat) {
    is_bool <- methods::is(mat, "lgCMatrix")
    nr <- nrow(mat); nc <- ncol(mat); nnz <- length(mat@x)
    indtype <- .indtype_for_size(max(nr, nc, nnz))
    grp <- root$create_group(key)
    .h5_write_index(grp, "colptr", as.integer(mat@p) + 1L, indtype)   # 0-based -> 1-based
    .h5_write_index(grp, "rowval", as.integer(mat@i) + 1L, indtype)
    if (is_bool) {
        if (!all(mat@x)) grp$create_dataset("nzval", robj = as.logical(mat@x), chunk_dims = NULL)
    } else {
        grp$create_dataset("nzval", robj = as.double(mat@x), chunk_dims = NULL)
    }
    invisible()
}

.h5_set_matrix <- function(daf, ra, ca, name, mat, overwrite) {
    mat <- .validate_matrix_value(daf, ra, ca, name, mat)
    if (!overwrite) .require_no_matrix(daf, ra, ca, name, relayout = FALSE)
    root <- .h5_root(daf); key <- .hkey_matrix(ra, ca, name)
    if (root$exists(key)) root$link_delete(key)
    if (methods::is(mat, "dgCMatrix") || methods::is(mat, "lgCMatrix")) {
        .h5_write_matrix_sparse(root, key, mat)
    } else {
        # Dense: hdf5r stores R dim (nr,nc) as HDF5 (nc,nr) col-major = Julia's
        # convention, so write directly (string matrices too).
        root$create_dataset(key, robj = as.matrix(mat), chunk_dims = NULL)
    }
    bump_matrix_counter(daf, ra, ca, name)
    MEMORY_DATA
}

.h5_delete_matrix <- function(daf, ra, ca, name, must_exist) {
    root <- .h5_root(daf); key <- .hkey_matrix(ra, ca, name)
    if (!root$exists(key)) {
        if (must_exist) .require_matrix(daf, ra, ca, name, relayout = FALSE)
        return(invisible())
    }
    root$link_delete(key)
    invisible()
}

local({
    for (cls in list(H5df, H5dfReadOnly)) {
        S7::method(format_has_matrix,
            list(cls, S7::class_character, S7::class_character, S7::class_character)) <-
            function(daf, rows_axis, columns_axis, name) .h5_has_matrix(daf, rows_axis, columns_axis, name)
        S7::method(format_matrices_set,
            list(cls, S7::class_character, S7::class_character)) <-
            function(daf, rows_axis, columns_axis) .h5_matrices_set(daf, rows_axis, columns_axis)
        S7::method(format_get_matrix,
            list(cls, S7::class_character, S7::class_character, S7::class_character)) <-
            function(daf, rows_axis, columns_axis, name) {
                m <- .h5_get_matrix_impl(daf, rows_axis, columns_axis, name)
                .cache_group_value(
                    .attach_matrix_axis_dimnames(daf, rows_axis, columns_axis, m),
                    .files_daf_classify_matrix(m))
            }
    }
})

S7::method(format_set_matrix,
    list(H5df, S7::class_character, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)) <-
    function(daf, rows_axis, columns_axis, name, mat, overwrite) {
        .h5_set_matrix(daf, rows_axis, columns_axis, name, mat, overwrite)
    }
S7::method(format_delete_matrix,
    list(H5df, S7::class_character, S7::class_character, S7::class_character, S7::class_logical)) <-
    function(daf, rows_axis, columns_axis, name, must_exist) {
        .h5_delete_matrix(daf, rows_axis, columns_axis, name, must_exist)
    }

# set_matrix()'s public wrapper (writers.R) calls format_relayout_matrix
# whenever rows_axis != columns_axis and relayout defaults to TRUE, so this
# must exist even though it wasn't spelled out in the matrices task; mirrors
# MemoryDaf/ZarrDaf: read, transpose, physically store at the flipped layout.
.h5_relayout_matrix <- function(daf, ra, ca, name) {
    src <- format_get_matrix(daf, ra, ca, name)$value
    transposed <- if (methods::is(src, "dgCMatrix") || methods::is(src, "lgCMatrix")) {
        Matrix::t(src)
    } else {
        t(src)
    }
    format_set_matrix(daf, ca, ra, name, transposed, overwrite = TRUE)
    invisible()
}
S7::method(format_relayout_matrix,
    list(H5df, S7::class_character, S7::class_character, S7::class_character)) <-
    function(daf, rows_axis, columns_axis, name) {
        .h5_relayout_matrix(daf, rows_axis, columns_axis, name)
    }
