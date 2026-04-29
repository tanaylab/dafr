#' @include classes.R files_daf.R zarr_format.R
NULL

#' Convert a `files_daf` directory to a `zarr_daf` directory.
#'
#' Both formats store identical raw little-endian numeric payloads on
#' disk. This conversion re-serializes only the JSON metadata and the
#' string blobs (which differ between formats: files-format uses
#' newline-delimited UTF-8, while Zarr uses `vlen-utf8`).
#'
#' Same-filesystem only. The function probes a hard-link from `src` to
#' `dst` up front; if the link fails (typically because the paths are
#' on different filesystems) the call errors with no automatic copy
#' fallback.
#'
#' Note: this implementation is correctness-first; the numeric blobs
#' are re-written through the regular `set_*` API rather than
#' hard-linked. The probe still enforces the same-filesystem
#' constraint so a future hard-link optimization can ship without an
#' API break.
#'
#' @param src Path to an existing `files_daf` directory.
#' @param dst Path to a non-existing destination directory; will be
#'   created. Errors if `dst` already exists.
#' @return Invisibly the destination path.
#' @examples
#' \dontrun{
#' files_to_zarr("/path/to/files.daf", "/path/to/converted.daf.zarr")
#' }
#' @export
files_to_zarr <- function(src, dst) {
    .convert_check_paths(src, dst)
    dir.create(dst, recursive = TRUE)
    success <- FALSE
    on.exit(
        if (!success) unlink(dst, recursive = TRUE, force = TRUE),
        add = TRUE
    )
    .convert_probe_link(src, dst)
    src_daf <- files_daf(src, mode = "r")
    dst_daf <- zarr_daf(dst, mode = "w")
    .convert_copy_contents(src_daf, dst_daf)
    success <- TRUE
    invisible(dst)
}

#' Convert a `zarr_daf` directory to a `files_daf` directory.
#'
#' Reverse of [files_to_zarr()]. Re-serializes JSON metadata and
#' string blobs. Same-filesystem only — cross-filesystem hard-links
#' fail and produce an error (no automatic copy fallback).
#'
#' @inheritParams files_to_zarr
#' @return Invisibly the destination path.
#' @examples
#' \dontrun{
#' zarr_to_files("/path/to/source.daf.zarr", "/path/to/converted.daf")
#' }
#' @export
zarr_to_files <- function(src, dst) {
    .convert_check_paths(src, dst)
    dir.create(dst, recursive = TRUE)
    success <- FALSE
    on.exit(
        if (!success) unlink(dst, recursive = TRUE, force = TRUE),
        add = TRUE
    )
    .convert_probe_link(src, dst)
    src_daf <- zarr_daf(src, mode = "r")
    dst_daf <- files_daf(dst, mode = "w")
    .convert_copy_contents(src_daf, dst_daf)
    success <- TRUE
    invisible(dst)
}

# ---- Internals -----------------------------------------------------------

.convert_check_paths <- function(src, dst) {
    if (!dir.exists(src)) {
        stop(sprintf("source directory %s does not exist", sQuote(src)),
             call. = FALSE)
    }
    if (file.exists(dst)) {
        stop(sprintf(
            "destination %s already exists; refusing to overwrite",
            sQuote(dst)
        ), call. = FALSE)
    }
}

# Probe with a temporary file in src; try to hard-link into dst.
# If link fails (typically EXDEV across filesystems), error clearly.
.convert_probe_link <- function(src, dst) {
    probe_src <- tempfile(tmpdir = src, fileext = ".convert-probe")
    probe_dst <- file.path(dst, basename(probe_src))
    file.create(probe_src)
    on.exit(unlink(probe_src), add = TRUE)
    on.exit(unlink(probe_dst), add = TRUE)
    ok <- suppressWarnings(file.link(probe_src, probe_dst))
    if (!isTRUE(ok)) {
        stop(sprintf(
            paste0(
                "cannot hard-link from %s to %s — paths must be on ",
                "the same filesystem"
            ),
            sQuote(src), sQuote(dst)
        ), call. = FALSE)
    }
}

# Walk a source DafReader and write the same logical content into dst.
# Works in either direction (files <-> zarr) since both speak the
# common DafReader / DafWriter API.
.convert_copy_contents <- function(src_daf, dst_daf) {
    for (name in scalars_set(src_daf)) {
        set_scalar(dst_daf, name, get_scalar(src_daf, name))
    }
    axes <- axes_set(src_daf)
    for (axis in axes) {
        add_axis(dst_daf, axis, axis_vector(src_daf, axis))
    }
    for (axis in axes) {
        for (name in vectors_set(src_daf, axis)) {
            set_vector(
                dst_daf, axis, name,
                get_vector(src_daf, axis, name)
            )
        }
    }
    for (rows in axes) {
        for (cols in axes) {
            for (name in matrices_set(src_daf, rows, cols)) {
                set_matrix(
                    dst_daf, rows, cols, name,
                    get_matrix(src_daf, rows, cols, name)
                )
            }
        }
    }
    invisible()
}
