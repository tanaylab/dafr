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
