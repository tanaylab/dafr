# Zarr Store interface: a uniform read/write API for byte content
# keyed by path strings. Format-version-agnostic (the v3 metadata/chunk
# codec in R/zarr_v3.R is layered on top). Concrete impls in this file:
#   - DirStore (filesystem)
#   - DictStore (in-memory, env-backed)
#   - MmapZipStore (zip archive, mmap-backed; C++ in src/mmap_zip_store.cpp)
#
# Every Zarr metadata/chunk operation goes through this interface.
# Mirrors zarr-python's MutableMapping store: get / set / delete /
# exists / list-with-prefix.

#' Zarr Store abstract base class.
#'
#' Uniform read/write API for byte content keyed by path strings.
#' Concrete implementations: [DirStore], [DictStore], [MmapZipStore].
#'
#' @param root (`DirStore`) Filesystem root path; created if missing.
#' @param env (`DictStore`) Internal environment used as the key-value
#'   backing store; typically created by [new_dict_store()].
#' @param path (`MmapZipStore`) Filesystem path to a zip archive.
#' @param mode (`MmapZipStore`) One of `"r"`, `"r+"`, `"w+"`, `"w"`;
#'   set by [new_mmap_zip_store()].
#' @param xptr (`MmapZipStore`) Internal external pointer to the C++
#'   store; set by [new_mmap_zip_store()] and not intended for direct
#'   use.
#' @param consolidate_cache (`DirStore`/`DictStore`) Internal environment
#'   holding the in-memory consolidated-metadata index, maintained
#'   incrementally across writes so `set_*` need not re-parse the whole
#'   store root on every mutation. Created automatically; not intended for
#'   direct use.
#' @name ZarrStore
#' @export
ZarrStore <- S7::new_class("ZarrStore", abstract = TRUE)

#' @rdname ZarrStore
#' @export
DirStore <- S7::new_class(
    "DirStore",
    parent = ZarrStore,
    properties = list(
        root = S7::class_character,
        # In-memory consolidated-metadata index (`$cmeta`), maintained
        # incrementally by R/zarr_v3.R so set_* does not re-parse the whole
        # store's root on every mutation. Per-instance; rebuilt from disk on
        # first use if empty.
        consolidate_cache = S7::new_property(
            S7::class_environment,
            default = quote(new.env(parent = emptyenv()))
        )
    )
)

#' @rdname ZarrStore
#' @export
DictStore <- S7::new_class(
    "DictStore",
    parent = ZarrStore,
    properties = list(
        env = S7::class_any,
        # See DirStore$consolidate_cache.
        consolidate_cache = S7::new_property(
            S7::class_environment,
            default = quote(new.env(parent = emptyenv()))
        )
    )
)

#' @rdname ZarrStore
#' @export
MmapZipStore <- S7::new_class(
    "MmapZipStore",
    parent = ZarrStore,
    properties = list(
        path = S7::class_character,
        mode = S7::class_character,
        xptr = S7::class_any
    )
)

# Constructors.

#' Create a directory-backed Zarr store.
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

#' Create an in-memory Zarr store.
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

#' Create a zip-archive-backed Zarr store (mmap, append-only).
#'
#' Opens (or creates) a single ZIP archive at `path` as a Zarr store.
#' Reads use a shared mmap of the archive (zero-copy for stored entries
#' via ALTREP RAW); writes append entries with a crash-safe two-step
#' commit protocol. Mirrors upstream
#' `DataAxesFormats.MmapZipStores.MmapZipStore`.
#'
#' Modes: `"r"` (read existing), `"r+"` (read/write existing),
#' `"w+"` (read/write, create if missing), `"w"` (truncate + create).
#' On a writable open, the store reserves `max_file_size` bytes of
#' virtual address space (no RAM cost) and grows the file via
#' `ftruncate` as entries append. Default `max_file_size` is 1 TiB.
#'
#' @param path Filesystem path to a `.daf.zarr.zip` archive.
#' @param mode One of `"r"`, `"r+"`, `"w+"`, `"w"`.
#' @param max_file_size Cap on the writable virtual reservation, in
#'   bytes. Ignored for read-only opens. Defaults to 1 TiB.
#' @return A `MmapZipStore`.
#' @examples
#' \dontrun{
#' s <- new_mmap_zip_store("/path/to/foo.daf.zarr.zip", mode = "w")
#' }
#' @export
new_mmap_zip_store <- function(path, mode = "r", max_file_size = 2^40) {
    stopifnot(
        is.character(path), length(path) == 1L, nzchar(path),
        is.character(mode), length(mode) == 1L,
        mode %in% c("r", "r+", "w+", "w"),
        is.numeric(max_file_size), length(max_file_size) == 1L,
        max_file_size > 0
    )
    xptr <- dafr_mmap_zip_open(path, mode, as.numeric(max_file_size))
    MmapZipStore(path = path, mode = mode, xptr = xptr)
}

# Generics.

#' Zarr store generics.
#'
#' Low-level byte-level operations on a [ZarrStore]. All Zarr I/O
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
#' @param ... Not used; present for S7 generic dispatch compatibility.
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

# MmapZipStore impls (read-only at slice 17 phase 2; writer lands phase 4).
S7::method(store_get_bytes, list(MmapZipStore, S7::class_character)) <-
    function(store, path) {
        dafr_mmap_zip_get_bytes(S7::prop(store, "xptr"), path)
    }

S7::method(store_exists, list(MmapZipStore, S7::class_character)) <-
    function(store, path) {
        dafr_mmap_zip_exists(S7::prop(store, "xptr"), path)
    }

S7::method(store_list, list(MmapZipStore, S7::class_character)) <-
    function(store, prefix) {
        dafr_mmap_zip_list(S7::prop(store, "xptr"), prefix)
    }

S7::method(store_set_bytes, list(MmapZipStore, S7::class_character, S7::class_any)) <-
    function(store, path, bytes) {
        dafr_mmap_zip_set_bytes(S7::prop(store, "xptr"), path, as.raw(bytes))
        invisible()
    }

# MmapZipStore is append-only: deletion is unsupported by design (it's a
# zip archive, not a file system). The C++ entry point throws a clear
# error message; the R-side wrapper just calls into it.
S7::method(store_delete, list(MmapZipStore, S7::class_character)) <-
    function(store, path) {
        dafr_mmap_zip_delete(S7::prop(store, "xptr"), path)
        invisible()
    }
