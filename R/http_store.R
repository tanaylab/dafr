#' @include zarr_store.R http_client.R
NULL

#' Read-only HTTP-served Zarr store.
#'
#' Implements the [ZarrStore] interface over HTTP(S). Used by
#' [zarr_daf()] when given an `http(s)://` URL pointing at a `.daf.zarr`
#' directory served by a static web server. Reads `.zmetadata`
#' (consolidated metadata) once at open; subsequent `store_exists` /
#' `store_list` resolve against the parsed dictionary, and chunk fetches
#' are cached in process memory.
#'
#' Writes hard-error: HTTP-served Zarr is read-only.
#'
#' @param url Root URL of the served Zarr v2 directory.
#' @param zmetadata Parsed consolidated `.zmetadata` content.
#' @param chunk_cache Environment caching fetched chunk bytes by path.
#' @export
HttpStore <- S7::new_class(
    name    = "HttpStore",
    package = "dafr",
    parent  = ZarrStore,
    properties = list(
        url           = S7::class_character,
        zmetadata     = S7::class_list,
        chunk_cache   = S7::class_environment
    )
)

#' Construct a read-only [ZarrStore] over HTTP.
#'
#' @param url Root URL of a Zarr v2 directory served over HTTP(S).
#' @return An `HttpStore`.
#' @export
new_http_store <- function(url) {
    stopifnot(is.character(url), length(url) == 1L, !is.na(url))
    url <- sub("/+$", "", url)
    raw <- .dafr_http_get(paste0(url, "/.zmetadata"), allow_404 = TRUE)
    if (is.null(raw)) {
        stop(sprintf(paste0("HttpStore: %s/.zmetadata not found.\n",
                            "Remote Zarr stores require consolidated metadata."),
                     url), call. = FALSE)
    }
    z <- jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
    cache <- new.env(parent = emptyenv())
    # Cache the .zmetadata bytes so the first store_get_bytes(".zmetadata")
    # round-trip is a memory hit, not a re-fetch.
    assign(".zmetadata", raw, envir = cache)
    HttpStore(url = url, zmetadata = z, chunk_cache = cache)
}

# ---- Helpers ---------------------------------------------------------------

# Names of keys present in the consolidated .zmetadata dict (e.g.
# "scalars/version/.zarray"). Chunks are NOT listed here.
.http_store_meta_keys <- function(store) {
    z <- S7::prop(store, "zmetadata")
    meta <- z$metadata
    if (is.null(meta)) return(character(0L))
    names(meta)
}

# ---- store_* methods -------------------------------------------------------

S7::method(store_get_bytes, list(HttpStore, S7::class_character)) <-
    function(store, path) {
        cache <- S7::prop(store, "chunk_cache")
        if (exists(path, envir = cache, inherits = FALSE)) {
            return(get(path, envir = cache, inherits = FALSE))
        }
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
        if (path == ".zmetadata") return(TRUE)
        if (path %in% .http_store_meta_keys(store)) return(TRUE)
        # Fallback for keys not in .zmetadata (e.g. chunk paths). Try a
        # HEAD before giving up.
        .dafr_http_head(paste0(S7::prop(store, "url"), "/", path))
    }

S7::method(store_list, list(HttpStore, S7::class_character)) <-
    function(store, prefix) {
        keys <- .http_store_meta_keys(store)
        if (!nzchar(prefix)) return(keys)
        # DirStore returns full paths under the prefix; mirror that.
        full_prefix <- paste0(prefix, "/")
        keys[startsWith(keys, full_prefix)]
    }
