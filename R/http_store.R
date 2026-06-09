#' @include zarr_store.R http_client.R
NULL

#' Read-only HTTP-served Zarr v3 store.
#'
#' Implements the [ZarrStore] interface over HTTP(S). Used by
#' [zarr_daf()] when given an `http(s)://` URL pointing at a `.daf.zarr`
#' directory served by a static web server. Reads the **Zarr v3** inline
#' consolidated metadata from the root `zarr.json` once at open: v3 stores
#' carry `consolidated_metadata.metadata` (a dict keyed by node path) in the
#' root group instead of the v2 `.zmetadata` file. Subsequent `store_exists`
#' and `store_list` resolve against that index, node `zarr.json` reads are
#' served from it (re-serialized to the bytes the reader would otherwise have
#' fetched per node), and chunk fetches are cached in process memory.
#'
#' Writes hard-error: HTTP-served Zarr is read-only.
#'
#' Zarr version: this backend targets the **Zarr v3** on-disk layout that
#' `dafr` reads and writes locally (DataAxesFormats.jl 0.3.0). A legacy v2
#' store (root `.zmetadata`, no root `zarr.json`) is rejected on open with a
#' `python -m zarr v2_to_v3` conversion hint, mirroring the local `DirStore` /
#' `MmapZipStore` v2 rejection. Note this is distinct from `http_daf()` (the
#' FilesFormat-over-HTTP path in [http_daf()]), which uses `metadata.zip` and
#' is unaffected.
#'
#' @param url Root URL of the served Zarr v3 directory.
#' @param index Named list: the v3 node metadata index parsed from the root
#'   `zarr.json` `consolidated_metadata.metadata` (keyed by node path, with no
#'   `/zarr.json` suffix).
#' @param chunk_cache Environment caching fetched chunk bytes (and the root
#'   `zarr.json`) by path.
#' @export
HttpStore <- S7::new_class(
    name    = "HttpStore",
    package = "dafr",
    parent  = ZarrStore,
    properties = list(
        url           = S7::class_character,
        index         = S7::class_list,
        chunk_cache   = S7::class_environment
    )
)

#' Construct a read-only [ZarrStore] over HTTP (Zarr v3).
#'
#' GETs the root `zarr.json` and parses its inline
#' `consolidated_metadata.metadata` as the node index, so it targets the
#' **Zarr v3** layout that `dafr` writes. A legacy v2 store (root `.zmetadata`,
#' no `zarr.json`) is rejected with a `python -m zarr v2_to_v3` conversion
#' hint; a v3 store that lacks inline consolidated metadata cannot be
#' enumerated over HTTP and is likewise rejected. See [HttpStore] for the
#' supported alternatives.
#'
#' @param url Root URL of a Zarr v3 directory served over HTTP(S).
#' @return An `HttpStore`.
#' @export
new_http_store <- function(url) {
    stopifnot(is.character(url), length(url) == 1L, !is.na(url))
    url <- sub("/+$", "", url)

    raw <- .dafr_http_get(paste0(url, "/zarr.json"), allow_404 = TRUE)
    if (is.null(raw)) {
        # No root zarr.json. If the store still ships a v2 `.zmetadata`, name
        # the format mismatch and point at the conversion path; otherwise it
        # simply isn't a daf zarr store.
        if (.dafr_http_head(paste0(url, "/.zmetadata"))) {
            stop(sprintf(paste0(
                "HttpStore: %s is a Zarr v2 store; dafr requires a Zarr v3 ",
                "store (DAF 0.3.0). Convert via `python -m zarr v2_to_v3 ",
                "<path>` (zarr-python 3.1.2+) and re-serve."), url),
                call. = FALSE)
        }
        stop(sprintf("HttpStore: %s/zarr.json not found; not a Zarr v3 store.",
                     url), call. = FALSE)
    }

    root <- jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
    cm <- root$consolidated_metadata
    if (is.null(cm) || is.null(cm$metadata)) {
        stop(sprintf(paste0(
            "HttpStore: %s/zarr.json carries no inline consolidated_metadata; ",
            "a Zarr v3 store served over HTTP must include it so nodes can be ",
            "discovered without per-node requests."), url), call. = FALSE)
    }
    index <- cm$metadata
    if (!is.list(index)) index <- as.list(index)

    cache <- new.env(parent = emptyenv())
    # Cache the root zarr.json bytes so the first store_get_bytes("zarr.json")
    # round-trip is a memory hit, not a re-fetch.
    assign("zarr.json", raw, envir = cache)
    HttpStore(url = url, index = index, chunk_cache = cache)
}

# ---- Helpers ---------------------------------------------------------------

# Node paths present in the v3 consolidated index (no `/zarr.json` suffix),
# e.g. "scalars", "scalars/name", "vectors/cell/score". Chunks are NOT listed.
.http_store_index_paths <- function(store) {
    names(S7::prop(store, "index"))
}

# ---- store_* methods -------------------------------------------------------

S7::method(store_get_bytes, list(HttpStore, S7::class_character)) <-
    function(store, path) {
        cache <- S7::prop(store, "chunk_cache")
        if (exists(path, envir = cache, inherits = FALSE)) {
            return(get(path, envir = cache, inherits = FALSE))
        }
        # Node metadata is served from the consolidated index, re-serialized to
        # the same JSON the reader would have fetched per node. The root
        # zarr.json is cached at open and handled by the cache hit above; a key
        # of exactly "zarr.json" therefore never reaches the index branch.
        if (endsWith(path, "/zarr.json")) {
            node_path <- sub("/zarr.json$", "", path)
            node <- S7::prop(store, "index")[[node_path]]
            if (is.null(node)) return(NULL)
            bytes <- charToRaw(as.character(jsonlite::toJSON(
                node, auto_unbox = TRUE, null = "null", pretty = FALSE)))
            assign(path, bytes, envir = cache)
            return(bytes)
        }
        # Chunk bytes (and the root zarr.json on a cache miss) are fetched over
        # HTTP and cached.
        bytes <- .dafr_http_get(paste0(S7::prop(store, "url"), "/", path),
                                allow_404 = TRUE)
        if (!is.null(bytes)) {
            assign(path, bytes, envir = cache)
        }
        bytes
    }

S7::method(store_set_bytes, list(HttpStore, S7::class_character, S7::class_any)) <-
    function(store, path, bytes) {
        stop("HttpStore is read-only; cannot set: ", path, call. = FALSE)
    }

S7::method(store_delete, list(HttpStore, S7::class_character)) <-
    function(store, path) {
        stop("HttpStore is read-only; cannot delete: ", path, call. = FALSE)
    }

S7::method(store_exists, list(HttpStore, S7::class_character)) <-
    function(store, path) {
        if (path == "zarr.json") return(TRUE)
        if (endsWith(path, "/zarr.json")) {
            node_path <- sub("/zarr.json$", "", path)
            return(node_path %in% .http_store_index_paths(store))
        }
        # Non-metadata keys (chunk paths, legacy v2 markers) aren't in the v3
        # index. Fall back to a HEAD so chunk existence still resolves.
        .dafr_http_head(paste0(S7::prop(store, "url"), "/", path))
    }

S7::method(store_list, list(HttpStore, S7::class_character)) <-
    function(store, prefix) {
        paths <- .http_store_index_paths(store)
        # Mirror DirStore, which lists the `zarr.json` files under the prefix.
        # Chunks are intentionally omitted: the v3 reader builds chunk paths
        # directly and never enumerates them via store_list.
        if (!nzchar(prefix)) {
            return(c("zarr.json", paste0(paths, "/zarr.json")))
        }
        full_prefix <- paste0(prefix, "/")
        under <- paths[paths == prefix | startsWith(paths, full_prefix)]
        paste0(under, "/zarr.json")
    }
