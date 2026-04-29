#' Open a Daf store by URI or path.
#'
#' Path/URL-aware factory that dispatches to the right backend. Today
#' supports `memory://` (or no path / NULL) for in-memory stores,
#' regular filesystem paths for `files_daf`, and `*.daf.zarr`
#' directories for `zarr_daf`. `*.daf.zarr.zip` errors with
#' "lands in slice 17"; `http(s)://` errors with "lands in slice 18".
#'
#' @param uri Path or URL. `memory://` (or `NULL` / empty string) for
#'   an in-memory store; a filesystem directory path for `files_daf`;
#'   a URL with a recognized scheme for future backends.
#' @param mode One of `"r"`, `"r+"`, `"w"`, `"w+"`. Required for
#'   `files_daf`; ignored for `memory_daf`.
#' @param name Optional name for the daf object. Default derived from
#'   the URI.
#' @param ... Reserved for backend-specific options.
#' @return A `DafReader` or `DafWriter` (subclass depends on backend
#'   and mode).
#' @examples
#' d <- open_daf("memory://", name = "demo")
#' add_axis(d, "cell", c("c1", "c2"))
#' @export
open_daf <- function(uri = NULL, mode = "r", name = NULL, ...) {
    if (is.null(uri) || identical(uri, "") || identical(uri, "memory://")) {
        return(memory_daf(name = name %||% "memory"))
    }
    if (!is.character(uri) || length(uri) != 1L || is.na(uri)) {
        stop("`uri` must be a single character scalar or NULL", call. = FALSE)
    }
    # Future backends — placeholders so callers don't get cryptic errors.
    if (endsWith(uri, ".h5df") || grepl(".h5dfs#", uri, fixed = TRUE)) {
        stop("H5df backend not supported yet", call. = FALSE)
    }
    # Zip-backed Zarr lands in slice 17 with the C++ MmapZipStore.
    if (grepl("\\.daf\\.zarr\\.zip(#.*)?$", uri)) {
        stop(sprintf(
            "open_daf: zarr-zip backend (%s) lands in slice 17; not yet supported",
            sQuote(uri)
        ), call. = FALSE)
    }
    if (grepl("\\.daf\\.zarr$", uri)) {
        return(zarr_daf(uri, mode = mode, name = name))
    }
    if (grepl("^https?://", uri)) {
        stop(sprintf(
            "open_daf: http backend (%s) lands in slice 18; not yet supported",
            sQuote(uri)
        ), call. = FALSE)
    }
    # Default: filesystem path → files_daf
    files_daf(uri, mode = mode, name = name)
}
