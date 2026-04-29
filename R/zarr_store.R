# Zarr v2 Store interface: a uniform read/write API for byte content
# keyed by path strings. Concrete impls in this file:
#   - DirStore (filesystem)
#   - DictStore (in-memory, env-backed)
#   - MmapZipStore (stub; errors with "lands in slice 17")
#
# Every Zarr v2 operation in R/zarr_v2.R goes through this interface.
# Mirrors zarr-python's MutableMapping store: get / set / delete /
# exists / list-with-prefix.

#' Zarr v2 Store abstract base class.
#'
#' Uniform read/write API for byte content keyed by path strings.
#' Concrete implementations: [DirStore], [DictStore], [MmapZipStore].
#'
#' @name ZarrStore
#' @export
ZarrStore <- S7::new_class("ZarrStore", abstract = TRUE)

#' @rdname ZarrStore
#' @export
DirStore <- S7::new_class(
    "DirStore",
    parent = ZarrStore,
    properties = list(root = S7::class_character)
)

#' @rdname ZarrStore
#' @export
DictStore <- S7::new_class(
    "DictStore",
    parent = ZarrStore,
    properties = list(env = S7::class_any)
)

#' @rdname ZarrStore
#' @export
MmapZipStore <- S7::new_class(
    "MmapZipStore",
    parent = ZarrStore,
    properties = list(path = S7::class_character)
)

# Constructors.

#' Create a directory-backed Zarr v2 store.
#'
#' Each key maps to a regular file under `root`. Directories are
#' created as needed. The root must be writable.
#'
#' @param root Filesystem path; created if missing.
#' @return A `DirStore`.
#' @examples
#' tmp <- tempfile()
#' s <- new_dir_store(tmp)
#' @export
new_dir_store <- function(root) {
    if (!dir.exists(root)) {
        dir.create(root, recursive = TRUE, showWarnings = FALSE)
    }
    DirStore(root = normalizePath(root, mustWork = TRUE))
}

#' Create an in-memory Zarr v2 store.
#'
#' Backed by an R environment keyed by path strings. Useful for tests
#' and for ZarrDaf instances that don't need persistence.
#'
#' @return A `DictStore`.
#' @examples
#' s <- new_dict_store()
#' @export
new_dict_store <- function() {
    DictStore(env = new.env(parent = emptyenv()))
}

#' Stub for the mmap-backed zip store (slice 17).
#'
#' Errors with a message pointing to slice 17. The constructor exists
#' so future routing in `open_daf` can dispatch on `*.daf.zarr.zip`
#' URIs without callers having to special-case the absence of the
#' implementation.
#'
#' @param path Filesystem path to a zip archive.
#' @return Throws.
#' @examples
#' \dontrun{
#' new_mmap_zip_store("/path/to/foo.daf.zarr.zip")
#' }
#' @export
new_mmap_zip_store <- function(path) {
    stop(sprintf(
        "MmapZipStore (%s) lands in slice 17; not yet supported",
        sQuote(path)
    ), call. = FALSE)
}

# Generics.

#' Zarr v2 store generics.
#'
#' Low-level byte-level operations on a [ZarrStore]. All Zarr v2 I/O
#' goes through these five generics so the storage backend is
#' interchangeable.
#'
#' - `store_get_bytes(store, path)` — return raw vector, or `NULL` if missing.
#' - `store_set_bytes(store, path, bytes)` — write raw vector; create dirs.
#' - `store_delete(store, path)` — remove key; silent no-op if absent.
#' - `store_exists(store, path)` — logical scalar.
#' - `store_list(store, prefix)` — character vector of keys with given prefix;
#'   `prefix = ""` lists all keys.
#'
#' @param store A [ZarrStore].
#' @param path  Character scalar key (slash-separated path string).
#' @param bytes Raw vector of bytes to write.
#' @param prefix Character scalar; `""` to list all keys.
#' @name zarr_store_generics
#' @return See individual descriptions above.
NULL

#' @rdname zarr_store_generics
#' @export
store_get_bytes <- S7::new_generic("store_get_bytes", c("store", "path"))

#' @rdname zarr_store_generics
#' @export
store_set_bytes <- S7::new_generic(
    "store_set_bytes", c("store", "path", "bytes")
)

#' @rdname zarr_store_generics
#' @export
store_delete    <- S7::new_generic("store_delete",    c("store", "path"))

#' @rdname zarr_store_generics
#' @export
store_exists    <- S7::new_generic("store_exists",    c("store", "path"))

#' @rdname zarr_store_generics
#' @export
store_list      <- S7::new_generic("store_list",      c("store", "prefix"))

# DirStore impls.
S7::method(store_get_bytes, list(DirStore, S7::class_character)) <-
    function(store, path) {
        full <- file.path(S7::prop(store, "root"), path)
        if (!file.exists(full)) return(NULL)
        readBin(full, what = "raw", n = file.size(full))
    }

S7::method(store_set_bytes, list(DirStore, S7::class_character, S7::class_any)) <-
    function(store, path, bytes) {
        full <- file.path(S7::prop(store, "root"), path)
        dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
        writeBin(as.raw(bytes), full, useBytes = TRUE)
        invisible()
    }

S7::method(store_delete, list(DirStore, S7::class_character)) <-
    function(store, path) {
        full <- file.path(S7::prop(store, "root"), path)
        if (file.exists(full)) file.remove(full)
        invisible()
    }

S7::method(store_exists, list(DirStore, S7::class_character)) <-
    function(store, path) {
        file.exists(file.path(S7::prop(store, "root"), path))
    }

S7::method(store_list, list(DirStore, S7::class_character)) <-
    function(store, prefix) {
        full_prefix <- if (nzchar(prefix)) {
            file.path(S7::prop(store, "root"), prefix)
        } else {
            S7::prop(store, "root")
        }
        if (!dir.exists(full_prefix)) return(character(0L))
        files <- list.files(full_prefix, recursive = TRUE,
                            include.dirs = FALSE, all.files = TRUE)
        # Skip "." and ".." artifacts (list.files shouldn't return them, but
        # belt-and-suspenders).
        files <- files[!files %in% c(".", "..")]
        if (nzchar(prefix)) {
            paste(prefix, files, sep = "/")
        } else {
            files
        }
    }

# DictStore impls.
S7::method(store_get_bytes, list(DictStore, S7::class_character)) <-
    function(store, path) {
        env <- S7::prop(store, "env")
        if (!exists(path, envir = env, inherits = FALSE)) return(NULL)
        get(path, envir = env, inherits = FALSE)
    }

S7::method(store_set_bytes, list(DictStore, S7::class_character, S7::class_any)) <-
    function(store, path, bytes) {
        assign(path, as.raw(bytes), envir = S7::prop(store, "env"))
        invisible()
    }

S7::method(store_delete, list(DictStore, S7::class_character)) <-
    function(store, path) {
        env <- S7::prop(store, "env")
        if (exists(path, envir = env, inherits = FALSE)) {
            rm(list = path, envir = env)
        }
        invisible()
    }

S7::method(store_exists, list(DictStore, S7::class_character)) <-
    function(store, path) {
        exists(path, envir = S7::prop(store, "env"), inherits = FALSE)
    }

S7::method(store_list, list(DictStore, S7::class_character)) <-
    function(store, prefix) {
        keys <- ls(S7::prop(store, "env"), all.names = TRUE)
        if (!nzchar(prefix)) return(keys)
        keys[startsWith(keys, paste0(prefix, "/"))]
    }
