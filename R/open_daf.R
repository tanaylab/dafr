#' Open a Daf store by URI or path.
#'
#' Path/URL-aware factory that dispatches to the right backend.
#' Supported URIs:
#' - `memory://` (or no path / `NULL`) — in-memory [memory_daf()].
#' - filesystem directory path — [files_daf()].
#' - `*.daf.zarr` or `*.daf.zarr.zip` (filesystem or HTTP) — [zarr_daf()].
#' - any other `http(s)://` URL — [http_daf()] (read-only HTTP-served
#'   FilesDaf).
#'
#' HTTP backends are read-only; modes other than `"r"` are rejected.
#' HTTP zip-archive URLs are not supported (open the underlying
#' `.daf.zarr` directory instead).
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
    if (grepl("^https?://", uri)) {
        if (!identical(mode, "r")) {
            stop(sprintf(
                "open_daf: HTTP backend is read-only; mode=%s rejected: %s",
                mode, uri
            ), call. = FALSE)
        }
        if (grepl("\\.daf\\.zarr\\.zip(#.*)?$", uri)) {
            stop(sprintf(paste0(
                "open_daf: HTTP zip-archive URLs are not supported.\n",
                "Open the underlying server-side .daf.zarr directory directly.\n",
                "Refused: %s"
            ), uri), call. = FALSE)
        }
        if (grepl("\\.daf\\.zarr/?$", uri)) {
            return(zarr_daf(uri, mode = "r", name = name))
        }
        return(http_daf(uri, name = name))
    }
    if (grepl("\\.daf\\.zarr\\.zip(#.*)?$", uri) || grepl("\\.daf\\.zarr$", uri)) {
        return(zarr_daf(uri, mode = mode, name = name))
    }
    # Default: filesystem path → files_daf
    files_daf(uri, mode = mode, name = name)
}
