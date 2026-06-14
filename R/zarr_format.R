#' @include classes.R zarr_store.R zarr_v3.R cache.R cache_group.R format_api.R utils.R
NULL

#' Zarr-backed Daf class.
#'
#' Concrete `DafWriter` subclass instantiated by [zarr_daf()] when
#' opened with mode `"w"` / `"w+"` / `"r+"`. Operates against any
#' [ZarrStore]; supported backends today: `DirStore` (filesystem),
#' `DictStore` (in-memory), `MmapZipStore` (zip archive).
#'
#' @inheritParams DafReader
#' @param store Internal [ZarrStore] instance (set by [zarr_daf()]);
#'   not intended for direct use.
#' @examples
#' tmp <- tempfile(fileext = ".daf.zarr")
#' d <- zarr_daf(tmp, mode = "w")
#' add_axis(d, "cell", c("c1", "c2"))
#' @export
ZarrDaf <- S7::new_class(
    name    = "ZarrDaf",
    package = "dafr",
    parent  = DafWriter,
    properties = list(
        store = ZarrStore
    )
)

#' Read-only Zarr-backed Daf class.
#'
#' Concrete `DafReadOnly` subclass instantiated by [zarr_daf()] when
#' opened with mode `"r"`. All mutating `format_*` generics reject
#' calls on this class with a clear "store opened read-only" error.
#'
#' @inheritParams DafReader
#' @param store Internal [ZarrStore] instance (set by [zarr_daf()]);
#'   not intended for direct use.
#' @examples
#' \dontrun{
#' d <- zarr_daf("/path/to/existing.daf.zarr", mode = "r")
#' inherits(d, "dafr::ZarrDafReadOnly")
#' }
#' @export
ZarrDafReadOnly <- S7::new_class(
    name    = "ZarrDafReadOnly",
    package = "dafr",
    parent  = DafReadOnly,
    properties = list(
        store = ZarrStore
    )
)

# ---- Constructor ---------------------------------------------------------

#' Open a Zarr-backed Daf store.
#'
#' Path-aware constructor that picks the right backing store. A path
#' ending in `.daf.zarr` (or any directory path) creates a `DirStore`.
#' `:memory:` or `NULL` creates a `DictStore`. A path ending in
#' `.daf.zarr.zip` creates an `MmapZipStore` (mmap-backed zip).
#'
#' @param uri Filesystem directory path, `:memory:`, or `NULL`.
#' @param mode `"r"`, `"r+"`, `"w"`, or `"w+"`. `"w"` initializes a
#'   fresh empty store (creating the directory if needed). `"w+"`
#'   truncates any existing store and creates a fresh one. `"r+"`
#'   opens for read-write without truncating. `"r"` opens read-only.
#' @param name Optional name; default derived from `uri`.
#' @return A `ZarrDaf` (writeable) or `ZarrDafReadOnly`.
#' @examples
#' tmp <- tempfile(fileext = ".daf.zarr")
#' d <- zarr_daf(tmp, mode = "w")
#' add_axis(d, "cell", c("c1", "c2"))
#' @export
zarr_daf <- function(uri = NULL, mode = c("r", "r+", "w", "w+"),
                     name = NULL) {
    mode <- match.arg(mode)
    is_memory <- is.null(uri) || identical(uri, ":memory:") ||
        identical(uri, "memory://") || !nzchar(uri)
    is_http <- !is_memory && is.character(uri) && length(uri) == 1L &&
        grepl("^https?://", uri)
    if (is_http) {
        if (mode != "r") {
            stop(sprintf("zarr_daf: HTTP URLs are read-only; mode=%s rejected: %s",
                         mode, uri), call. = FALSE)
        }
        store <- new_http_store(uri)
        store_path <- sub("/+$", "", uri)
    } else if (is_memory) {
        store <- new_dict_store()
        store_path <- ":memory:"
    } else if (grepl("\\.daf\\.zarr\\.zip(#.*)?$", uri)) {
        # Strip any URL-style fragment (`#...`) before passing to the
        # filesystem-level store; only the path component identifies the
        # zip archive.
        zip_path <- sub("#.*$", "", uri)
        if (.is_windows()) {
            stop(sprintf(
                "zarr_daf: .daf.zarr.zip stores are not supported on Windows (path: %s). The slice-17 MmapZipStore is POSIX-only; use the unzipped .daf.zarr directory store instead, or run on Linux/macOS.",
                sQuote(zip_path)
            ), call. = FALSE)
        }
        if (mode == "r" && !file.exists(zip_path)) {
            stop(sprintf("zarr_daf: store does not exist at %s", sQuote(zip_path)),
                call. = FALSE
            )
        }
        # Map zarr_daf semantics onto MmapZipStore's modes:
        #   - "r"  : read existing archive.
        #   - "r+" : read+write existing archive (no creation).
        #   - "w"  : truncate+create (always wipe any existing file).
        #   - "w+" : create-if-missing, but if it already exists, match
        #            the directory branch (which DOES `unlink` first) so
        #            that user-visible behaviour is consistent across
        #            DirStore vs MmapZipStore.
        store_mode <- mode
        if (mode == "w+" && file.exists(zip_path)) {
            store_mode <- "w"  # truncate+create, mirrors `unlink` of the dir branch.
        }
        store <- new_mmap_zip_store(zip_path, mode = store_mode)
        store_path <- zip_path
    } else {
        if (mode == "r" && !dir.exists(uri)) {
            stop(sprintf("zarr_daf: store does not exist at %s", sQuote(uri)),
                call. = FALSE
            )
        }
        if (mode == "w+" && dir.exists(uri)) {
            unlink(uri, recursive = TRUE, force = TRUE)
        }
        if (!dir.exists(uri)) {
            dir.create(uri, recursive = TRUE, showWarnings = FALSE)
        }
        store <- new_dir_store(uri)
        store_path <- uri
    }

    if (mode %in% c("w", "w+") && !.zarr_daf_marker_exists(store)) {
        .zarr_daf_init_store(store)
    }

    if (mode %in% c("r", "r+")) {
        .zarr_reject_if_v2(store, store_path)
        if (!.zarr_daf_marker_exists(store)) {
            stop(sprintf(
                "zarr_daf: store at %s is not a valid ZarrDaf store (missing `daf` marker)",
                sQuote(store_path)
            ), call. = FALSE)
        }
        .zarr_verify_daf(store, store_path)
    }

    final_name <- if (!is.null(name)) name else basename(store_path)
    cls <- if (mode == "r") ZarrDafReadOnly else ZarrDaf
    internal <- new_internal_env()
    internal$path <- store_path  # so complete_path(daf) and description() see it
    internal$mode <- mode
    cls(
        name                   = final_name,
        store                  = store,
        internal               = internal,
        cache                  = new_cache_env(),
        axis_version_counter   = new_counter_env(),
        vector_version_counter = new_counter_env(),
        matrix_version_counter = new_counter_env()
    )
}

# Daf-zarr format version is carried as the root group attribute `daf:[MAJOR,
# MINOR]` (DataAxesFormats.jl 0.3.0, Zarr v3). See R/zarr_v3.R for the marker.
.ZARR_DAF_MAJOR <- .ZARR_V3_DAF_MAJOR
.ZARR_DAF_MINOR <- .ZARR_V3_DAF_MINOR

# Does the store carry a v3 daf marker (root group with a `daf` attribute)?
.zarr_daf_marker_exists <- function(store) {
    zarr_v3_daf_marker_exists(store)
}

# Reject a legacy Zarr v2 store (DAF 0.3.0 does the same). Detected by a root
# `.zgroup`/`.zarray` with no `zarr.json`.
.zarr_reject_if_v2 <- function(store, store_path) {
    has_v3 <- store_exists(store, "zarr.json")
    has_v2 <- store_exists(store, ".zgroup") || store_exists(store, ".zarray") ||
              store_exists(store, "daf/.zarray")
    if (!has_v3 && has_v2) {
        stop(sprintf(paste0(
            "zarr_daf: Zarr v2 store at %s; dafr requires a Zarr v3 store ",
            "(DAF 0.3.0). Convert via `python -m zarr v2_to_v3 <path>` ",
            "(zarr-python 3.1.2+), then reopen."), sQuote(store_path)),
            call. = FALSE)
    }
    invisible()
}

# Read + validate the root daf version attribute. A newer major, or a newer
# minor than this code supports, is rejected.
.zarr_verify_daf <- function(store, store_path) {
    version <- zarr_v3_daf_version(store)
    if (length(version) != 2L) {
        stop(sprintf("zarr_daf: store at %s has a malformed `daf` marker",
                     sQuote(store_path)), call. = FALSE)
    }
    if (version[1L] != .ZARR_DAF_MAJOR || version[2L] > .ZARR_DAF_MINOR) {
        stop(sprintf(paste0(
            "zarr_daf: incompatible format version %d.%d for the daf zarr ",
            "store at %s; this code supports version %d.%d"),
            version[1L], version[2L], sQuote(store_path),
            .ZARR_DAF_MAJOR, .ZARR_DAF_MINOR), call. = FALSE)
    }
    invisible()
}

# Initialize an empty v3 store: root group (with daf attribute) + the four
# container groups, then refresh consolidated metadata.
.zarr_daf_init_store <- function(store) {
    zarr_v3_write_root(store)
    for (grp in c("scalars", "axes", "vectors", "matrices")) {
        zarr_v3_write_group(store, grp)
    }
    zarr_v3_write_consolidated(store)
    invisible()
}

# ---- read-only guard for ZarrDafReadOnly --------------------------------

.zarr_read_only_guard <- function(verb) {
    stop(
        sprintf("zarr_daf: store opened read-only; %s not permitted", verb),
        call. = FALSE
    )
}

# ---- Scalars -------------------------------------------------------------

S7::method(format_has_scalar, list(ZarrDaf, S7::class_character)) <-
    function(daf, name) {
        !is.null(zarr_v3_read_array(
            S7::prop(daf, "store"),
            paste0("scalars/", name)
        ))
    }
S7::method(format_has_scalar, list(ZarrDafReadOnly, S7::class_character)) <-
    function(daf, name) {
        !is.null(zarr_v3_read_array(
            S7::prop(daf, "store"),
            paste0("scalars/", name)
        ))
    }

.zarr_get_scalar <- function(daf, name) {
    store <- S7::prop(daf, "store")
    base <- paste0("scalars/", name)
    meta <- zarr_v3_read_array(store, base)
    if (is.null(meta)) {
        .require_scalar(daf, name)
    }
    n <- as.integer(meta$shape[[1L]])
    bytes <- store_get_bytes(store, zarr_v3_chunk_path(base, 1L))
    if (is.null(bytes)) {
        stop(sprintf("scalar %s missing chunk", sQuote(name)), call. = FALSE)
    }
    decoded <- if (identical(meta$data_type, "string")) {
        zarr_v3_decode_strings(bytes, n = n)
    } else {
        zarr_v3_decode_chunk(bytes, meta$data_type, n = n)
    }
    decoded[[1L]]   # scalars are shape [1]
}

S7::method(format_get_scalar, list(ZarrDaf, S7::class_character)) <-
    function(daf, name) {
        .cache_group_value(.zarr_get_scalar(daf, name), MEMORY_DATA)
    }
S7::method(format_get_scalar, list(ZarrDafReadOnly, S7::class_character)) <-
    function(daf, name) {
        .cache_group_value(.zarr_get_scalar(daf, name), MEMORY_DATA)
    }

S7::method(format_scalars_set, ZarrDaf) <- function(daf) {
    .zarr_scalars_set(daf)
}
S7::method(format_scalars_set, ZarrDafReadOnly) <- function(daf) {
    .zarr_scalars_set(daf)
}
.zarr_scalars_set <- function(daf) {
    store <- S7::prop(daf, "store")
    keys <- store_list(store, "scalars")
    if (length(keys) == 0L) return(character(0L))
    rel <- sub("^scalars/", "", keys)
    # A scalar is "<name>/zarr.json" (one slash); skip the container marker.
    names_only <- sub("/zarr.json$", "", rel[grepl("^[^/]+/zarr.json$", rel)])
    sort(unique(names_only), method = "radix")
}

S7::method(
    format_set_scalar,
    list(ZarrDaf, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, name, value, overwrite) {
    .assert_scalar_value(name, value)
    if (!overwrite) {
        .require_no_scalar(daf, name)
    }
    store <- S7::prop(daf, "store")
    .zarr_write_scalar(store, name, value)
    MEMORY_DATA
}
S7::method(
    format_set_scalar,
    list(ZarrDafReadOnly, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, name, value, overwrite) {
    .zarr_read_only_guard("set_scalar")
}

# Write a single dense v3 array (numeric or string) + its one chunk. ndim is
# derived from length(shape); caller refreshes consolidated metadata.
.zarr_write_dense_array <- function(store, base, values, shape) {
    dtype <- zarr_v3_dtype_for_r(values)
    zarr_v3_write_array(store, base, zarr_v3_array_meta(shape = shape, dtype = dtype))
    chunk <- if (dtype == "string") zarr_v3_encode_strings(values) else
        zarr_v3_encode_chunk(values, dtype)
    store_set_bytes(store, zarr_v3_chunk_path(base, length(shape)), chunk)
    invisible()
}

.zarr_write_scalar <- function(store, name, value) {
    base <- paste0("scalars/", name)
    .zarr_write_dense_array(store, base, value, length(value))
    zarr_v3_consolidate_upsert(store, base)
}

S7::method(
    format_delete_scalar,
    list(ZarrDaf, S7::class_character, S7::class_logical)
) <- function(daf, name, must_exist) {
    store <- S7::prop(daf, "store")
    base <- paste0("scalars/", name)
    if (!store_exists(store, paste0(base, "/zarr.json"))) {
        if (must_exist) {
            .require_scalar(daf, name)
        }
        return(invisible())
    }
    store_delete(store, paste0(base, "/zarr.json"))
    store_delete(store, zarr_v3_chunk_path(base, 1L))
    zarr_v3_write_consolidated(store)
    invisible()
}
S7::method(
    format_delete_scalar,
    list(ZarrDafReadOnly, S7::class_character, S7::class_logical)
) <- function(daf, name, must_exist) {
    .zarr_read_only_guard("delete_scalar")
}

# ---- Axes ----------------------------------------------------------------

S7::method(format_has_axis, list(ZarrDaf, S7::class_character)) <-
    function(daf, axis) {
        !is.null(zarr_v3_read_array(
            S7::prop(daf, "store"),
            paste0("axes/", axis)
        ))
    }
S7::method(format_has_axis, list(ZarrDafReadOnly, S7::class_character)) <-
    function(daf, axis) {
        !is.null(zarr_v3_read_array(
            S7::prop(daf, "store"),
            paste0("axes/", axis)
        ))
    }

S7::method(format_axes_set, ZarrDaf) <- function(daf) .zarr_axes_set(daf)
S7::method(format_axes_set, ZarrDafReadOnly) <- function(daf) .zarr_axes_set(daf)
.zarr_axes_set <- function(daf) {
    store <- S7::prop(daf, "store")
    keys <- store_list(store, "axes")
    if (length(keys) == 0L) return(character(0L))
    rel <- sub("^axes/", "", keys)
    # An axis is "<name>/zarr.json" (one slash); skip the container marker.
    names_only <- sub("/zarr.json$", "", rel[grepl("^[^/]+/zarr.json$", rel)])
    sort(unique(names_only), method = "radix")
}

.zarr_axis_entries <- function(daf, axis) {
    # Memoize the vlen-utf8 decode. Decoding the axis-name strings from the
    # store is ~45% of a dense matrix-query's time on a 4000+2500 fixture and
    # was paid afresh for every distinct query (only an exact-repeat query hit
    # the QueryData result cache). Cache the decoded vector at the "memory"
    # tier keyed by cache_key_axis + axis_stamp: distinct queries over the same
    # axes now decode once. axis_stamp bumps on delete_axis, so a deleted +
    # recreated axis invalidates correctly (the exact contract the vector /
    # matrix caches already use). Chains/views over a ZarrDaf delegate
    # format_axis_array down to here, so they inherit the cache too.
    cache_env <- S7::prop(daf, "cache")
    key <- cache_key_axis(axis)
    stamp_now <- axis_stamp(daf, axis)
    hit <- cache_lookup(cache_env, "memory", key, stamp_now)
    if (!is.null(hit)) return(hit)

    store <- S7::prop(daf, "store")
    base <- paste0("axes/", axis)
    meta <- zarr_v3_read_array(store, base)
    if (is.null(meta)) {
        .require_axis(daf, "for: zarr backend", axis)
    }
    chunk_bytes <- store_get_bytes(store, zarr_v3_chunk_path(base, 1L))
    if (is.null(chunk_bytes)) {
        stop(sprintf("axis %s missing chunk", sQuote(axis)), call. = FALSE)
    }
    n <- as.integer(meta$shape[[1L]])
    entries <- zarr_v3_decode_strings(chunk_bytes, n = n)
    cache_store(cache_env, "memory", key, entries, stamp_now,
        size_bytes = as.numeric(utils::object.size(entries)))
    entries
}

S7::method(format_axis_array, list(ZarrDaf, S7::class_character)) <-
    function(daf, axis) {
        .cache_group_value(.zarr_axis_entries(daf, axis), MEMORY_DATA)
    }
S7::method(format_axis_array, list(ZarrDafReadOnly, S7::class_character)) <-
    function(daf, axis) {
        .cache_group_value(.zarr_axis_entries(daf, axis), MEMORY_DATA)
    }

S7::method(format_axis_length, list(ZarrDaf, S7::class_character)) <-
    function(daf, axis) {
        length(.zarr_axis_entries(daf, axis))
    }
S7::method(format_axis_length, list(ZarrDafReadOnly, S7::class_character)) <-
    function(daf, axis) {
        length(.zarr_axis_entries(daf, axis))
    }

S7::method(format_axis_dict, list(ZarrDaf, S7::class_character)) <-
    function(daf, axis) {
        .zarr_axis_dict(daf, axis)
    }
S7::method(format_axis_dict, list(ZarrDafReadOnly, S7::class_character)) <-
    function(daf, axis) {
        .zarr_axis_dict(daf, axis)
    }
.zarr_axis_dict <- function(daf, axis) {
    entries <- .zarr_axis_entries(daf, axis)
    dict <- new.env(parent = emptyenv(), size = length(entries))
    for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
    dict
}

S7::method(
    format_add_axis,
    list(ZarrDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, entries) {
    if (anyNA(entries)) {
        stop(sprintf("axis %s entries contain NA", sQuote(axis)),
            call. = FALSE
        )
    }
    if (any(!nzchar(entries))) {
        stop(sprintf("axis %s entries contain empty strings", sQuote(axis)),
            call. = FALSE
        )
    }
    if (anyDuplicated(entries)) {
        stop(sprintf(
            "non-unique entries for new axis: %s\nin the daf data: %s",
            axis, S7::prop(daf, "name")
        ), call. = FALSE)
    }
    .require_no_axis(daf, axis)
    store <- S7::prop(daf, "store")
    base <- paste0("axes/", axis)
    .zarr_write_dense_array(store, base, entries, length(entries))
    zarr_v3_consolidate_upsert(store, base)
    invisible()
}
S7::method(
    format_add_axis,
    list(ZarrDafReadOnly, S7::class_character, S7::class_character)
) <- function(daf, axis, entries) {
    .zarr_read_only_guard("add_axis")
}

S7::method(
    format_delete_axis,
    list(ZarrDaf, S7::class_character, S7::class_logical)
) <- function(daf, axis, must_exist) {
    store <- S7::prop(daf, "store")
    base <- paste0("axes/", axis)
    if (!store_exists(store, paste0(base, "/zarr.json"))) {
        if (must_exist) {
            .require_axis(daf, "for: delete_axis", axis)
        }
        return(invisible())
    }
    store_delete(store, paste0(base, "/zarr.json"))
    store_delete(store, zarr_v3_chunk_path(base, 1L))
    zarr_v3_write_consolidated(store)
    bump_axis_counter(daf, axis)
    invisible()
}
S7::method(
    format_delete_axis,
    list(ZarrDafReadOnly, S7::class_character, S7::class_logical)
) <- function(daf, axis, must_exist) {
    .zarr_read_only_guard("delete_axis")
}

# ---- Vectors -------------------------------------------------------------

S7::method(format_has_vector,
           list(ZarrDaf, S7::class_character, S7::class_character)) <-
    function(daf, axis, name) {
        .zarr_has_vector(daf, axis, name)
    }
S7::method(format_has_vector,
           list(ZarrDafReadOnly, S7::class_character, S7::class_character)) <-
    function(daf, axis, name) {
        .zarr_has_vector(daf, axis, name)
    }
.zarr_has_vector <- function(daf, axis, name) {
    store <- S7::prop(daf, "store")
    # Dense (array) or sparse (group); both carry a zarr.json at `base`.
    node <- zarr_v3_read_node(store, paste0("vectors/", axis, "/", name))
    !is.null(node)
}

S7::method(format_vectors_set,
           list(ZarrDaf, S7::class_character)) <-
    function(daf, axis) .zarr_vectors_set(daf, axis)
S7::method(format_vectors_set,
           list(ZarrDafReadOnly, S7::class_character)) <-
    function(daf, axis) .zarr_vectors_set(daf, axis)
.zarr_vectors_set <- function(daf, axis) {
    store <- S7::prop(daf, "store")
    prefix <- paste0("vectors/", axis)
    keys <- store_list(store, prefix)
    if (length(keys) == 0L) return(character(0L))
    rel <- sub(paste0("^", prefix, "/"), "", keys)
    # A property (dense array or sparse group) is "<name>/zarr.json" (one
    # slash). Sparse children ("<name>/nzind/zarr.json") are deeper - skipped.
    names <- sub("/zarr.json$", "", rel[grepl("^[^/]+/zarr.json$", rel)])
    sort(unique(names), method = "radix")
}

# Filesystem path of a chunk, if the store backs it with a real file. Only the
# directory store does; the in-memory, zip, and HTTP stores return NULL so the
# caller falls back to decoding the chunk bytes.
.zarr_chunk_file <- function(store, chunk_key) {
    if (!S7::S7_inherits(store, DirStore)) return(NULL)
    full <- file.path(S7::prop(store, "root"), chunk_key)
    if (file.exists(full)) full else NULL
}

# Zero-copy fast path for a stored, uncompressed, single-chunk dense array:
# return an ALTREP mmap view of the chunk file when the dtype is mmap-able and
# the store is file-backed, else NULL (caller decodes). Mirrors the FilesDaf
# typed-mmap fast path (R/files_daf_read.R); chunk files start at offset 0 so
# they are page-aligned.
.zarr_try_mmap_dense <- function(store, chunk_key, dtype, n, compressor) {
    if (!isTRUE(dafr_opt("dafr.mmap"))) return(NULL)
    if (!is.null(compressor)) return(NULL)               # only stored/uncompressed
    if (n <= 0L) return(NULL)
    if (!dtype %in% c("float64", "int32")) return(NULL)  # mmap-able fixed-width
    file <- .zarr_chunk_file(store, chunk_key)
    if (is.null(file)) return(NULL)
    elt <- if (dtype == "float64") 8L else 4L
    if (file.size(file) < as.numeric(n) * elt) return(NULL)  # truncated -> decode/error
    if (dtype == "float64") mmap_real(file, n) else mmap_int(file, n)
}

# An mmap-backed (ALTREP) read is MAPPED_DATA: file-backed and cheap, it must
# stay lazy and not be copied into the capped in-memory cache tier. A decoded
# value (strings, compressed/zip/in-memory chunks) is a real MEMORY_DATA copy.
# Mirrors the FilesDaf classification (R/files_daf_read.R).
.zarr_value_cache_group <- function(value) {
    if (is_altrep_cpp(value)) MAPPED_DATA else MEMORY_DATA
}

# format_get_vector dispatches based on dense vs sparse layout on disk.
.zarr_get_vector <- function(daf, axis, name) {
    store <- S7::prop(daf, "store")
    base <- paste0("vectors/", axis, "/", name)
    node <- zarr_v3_read_node(store, base)
    if (is.null(node)) {
        .require_vector(daf, axis, name)
    }
    if (identical(node$node_type, "group")) {
        return(.zarr_get_sparse_vector(daf, axis, name))
    }
    # Dense (array) path.
    if (.zarr_is_sharded(node)) {
        return(.zarr_read_sharded_vector(store, base, node))
    }
    n <- as.integer(node$shape[[1L]])
    is_string <- identical(node$data_type, "string")
    if (!is_string) {
        mm <- .zarr_try_mmap_dense(store, zarr_v3_chunk_path(base, 1L),
                                   node$data_type, n, NULL)
        if (!is.null(mm)) return(mm)
    }
    chunk <- store_get_bytes(store, zarr_v3_chunk_path(base, 1L))
    if (is.null(chunk)) {
        # Zarr omits a chunk that is entirely fill_value (the all-fill
        # optimization). Reconstruct it from fill_value (matching Julia/Zarr.jl)
        # rather than erroring "missing chunk".
        fill <- node$fill_value
        if (is.null(fill)) fill <- if (is_string) "" else 0
        if (is_string) return(rep(as.character(fill), n))
        return(switch(zarr_v3_r_kind_for_dtype(node$data_type),
            double    = as.double(rep(fill, n)),
            integer   = as.integer(rep(fill, n)),
            integer64 = bit64::as.integer64(rep(fill, n)),
            logical   = as.logical(rep(fill, n)),
            rep(fill, n)))
    }
    if (is_string) {
        return(zarr_v3_decode_strings(chunk, n = n))
    }
    zarr_v3_decode_chunk(chunk, node$data_type, n = n)
}

.zarr_get_sparse_vector <- function(daf, axis, name) {
    # Returns a DENSE atomic vector to match the existing dafr convention
    # (FilesDaf densifies sparse vectors at the read boundary too — the
    # user-facing get_vector contract is "named atomic vector"). On-disk
    # layout is still sparse; densification happens on read.
    #
    # Upstream parity (matches sparse-matrix layout):
    #   - On disk the vector is a group holding `nzind`/`nzval` arrays. No
    #     length is stored; the vector's full length comes from the axis
    #     length.
    #   - The all-TRUE Bool case is inferred from the ABSENCE of
    #     `nzval/zarr.json` in the store.
    store <- S7::prop(daf, "store")
    base <- paste0("vectors/", axis, "/", name)
    n <- as.integer(format_axis_length(daf, axis))
    nzind_meta <- zarr_v3_read_array(store, paste0(base, "/nzind"))
    nzind <- .zarr_read_component_vector(store, paste0(base, "/nzind"), nzind_meta)
    # Upstream stores 1-based indices on disk (Julia SparseVector convention).
    # DAF writes nzind as int64, so the decode is integer64 -> as.integer().
    nzind_1 <- as.integer(nzind)

    has_nzval <- !is.null(zarr_v3_read_array(store, paste0(base, "/nzval")))
    if (!has_nzval) {
        # All-TRUE Bool sparse: nzval was omitted on write; synthesize.
        out <- logical(n)
        out[nzind_1] <- TRUE
        return(out)
    }
    nzval_meta <- zarr_v3_read_array(store, paste0(base, "/nzval"))
    # Non-bool sparse values are read as double here (DAF sparse values are
    # realistically float/bool); a genuine int64 sparse nzval would narrow to
    # double, lossy above 2^53 - future follow-up if needed.
    nzval <- .zarr_read_component_vector(store, paste0(base, "/nzval"), nzval_meta)
    out <- if (is.logical(nzval)) {
        logical(n)
    } else if (is.integer(nzval)) {
        integer(n)
    } else {
        numeric(n)
    }
    out[nzind_1] <- nzval
    out
}

S7::method(format_get_vector,
           list(ZarrDaf, S7::class_character, S7::class_character)) <-
    function(daf, axis, name) {
        v <- .attach_vector_axis_names(daf, axis,
            .zarr_get_vector(daf, axis, name))
        .cache_group_value(v, .zarr_value_cache_group(v))
    }
S7::method(format_get_vector,
           list(ZarrDafReadOnly, S7::class_character, S7::class_character)) <-
    function(daf, axis, name) {
        v <- .attach_vector_axis_names(daf, axis,
            .zarr_get_vector(daf, axis, name))
        .cache_group_value(v, .zarr_value_cache_group(v))
    }

# format_set_vector dispatches based on input type (dense vs sparse).
S7::method(
    format_set_vector,
    list(ZarrDaf, S7::class_character, S7::class_character,
         S7::class_any, S7::class_logical)
) <- function(daf, axis, name, vec, overwrite) {
    .require_axis(daf, sprintf("for the vector: %s", name), axis)
    if (!overwrite) {
        .require_no_vector(daf, axis, name)
    }
    n_axis <- format_axis_length(daf, axis)
    store <- S7::prop(daf, "store")
    base <- paste0("vectors/", axis, "/", name)
    existing <- zarr_v3_read_node(store, base)
    exists_dense <- !is.null(existing) &&
        identical(existing$node_type, "array")
    exists_sparse <- !is.null(existing) &&
        identical(existing$node_type, "group")
    # Validate length BEFORE deleting existing form, so a length mismatch
    # leaves the prior data intact.
    if (methods::is(vec, "sparseVector")) {
        if (vec@length != n_axis) {
            stop(sprintf(
                "vector %s length %d != axis %s length %d",
                sQuote(name), vec@length, sQuote(axis), n_axis
            ), call. = FALSE)
        }
    } else {
        if (length(vec) != n_axis) {
            stop(sprintf(
                "vector %s length %d != axis %s length %d",
                sQuote(name), length(vec), sQuote(axis), n_axis
            ), call. = FALSE)
        }
    }
    # Delete any existing form before overwriting.
    if (exists_dense) {
        store_delete(store, paste0(base, "/zarr.json"))
        store_delete(store, zarr_v3_chunk_path(base, 1L))
    }
    if (exists_sparse) {
        for (k in store_list(store, base)) store_delete(store, k)
    }
    if (methods::is(vec, "sparseVector")) {
        .zarr_write_sparse_vector(store, base, vec)
    } else {
        .zarr_write_dense_vector(store, base, vec)
    }
    bump_vector_counter(daf, axis, name)
    MEMORY_DATA
}
S7::method(
    format_set_vector,
    list(ZarrDafReadOnly, S7::class_character, S7::class_character,
         S7::class_any, S7::class_logical)
) <- function(daf, axis, name, vec, overwrite) {
    .zarr_read_only_guard("set_vector")
}

.zarr_write_dense_vector <- function(store, base, vec) {
    .zarr_write_dense_array(store, base, vec, length(vec))
    zarr_v3_consolidate_upsert(store, base)
}

.zarr_write_sparse_vector <- function(store, base, vec) {
    # Mark as a v3 group. NO attributes are written - upstream parity with
    # sparse matrices: shape comes from the axis length on read, and
    # the all-TRUE Bool case is inferred from absence of `nzval/`.
    zarr_v3_write_group(store, base)
    is_all_true_bool <- is.logical(vec@x) && length(vec@x) > 0L &&
                        all(vec@x, na.rm = FALSE)
    # Write nzind: int64, 1-based indices (DAF SparseVector convention).
    nzind <- bit64::as.integer64(vec@i)
    nzind_base <- paste0(base, "/nzind")
    zarr_v3_write_array(store, nzind_base,
                        zarr_v3_array_meta(shape = length(nzind),
                                           dtype = "int64"))
    store_set_bytes(store, zarr_v3_chunk_path(nzind_base, 1L),
                    zarr_v3_encode_chunk(nzind, "int64"))
    # Write nzval (skip for all-TRUE Bool — upstream-compatible compaction).
    if (!is_all_true_bool) {
        nzval_dtype <- zarr_v3_dtype_for_r(vec@x)
        nzval_base <- paste0(base, "/nzval")
        zarr_v3_write_array(store, nzval_base,
                            zarr_v3_array_meta(shape = length(vec@x),
                                               dtype = nzval_dtype))
        store_set_bytes(store, zarr_v3_chunk_path(nzval_base, 1L),
                        zarr_v3_encode_chunk(vec@x, nzval_dtype))
    }
    zarr_v3_consolidate_upsert(store, base)
}

S7::method(format_delete_vector,
           list(ZarrDaf, S7::class_character, S7::class_character,
                S7::class_logical)) <-
    function(daf, axis, name, must_exist) {
        store <- S7::prop(daf, "store")
        base <- paste0("vectors/", axis, "/", name)
        node <- zarr_v3_read_node(store, base)
        exists_dense <- !is.null(node) && identical(node$node_type, "array")
        exists_sparse <- !is.null(node) && identical(node$node_type, "group")
        if (!exists_dense && !exists_sparse) {
            if (must_exist) {
                .require_vector(daf, axis, name)
            }
            return(invisible())
        }
        if (exists_dense) {
            store_delete(store, paste0(base, "/zarr.json"))
            store_delete(store, zarr_v3_chunk_path(base, 1L))
        }
        if (exists_sparse) {
            for (k in store_list(store, base)) store_delete(store, k)
        }
        zarr_v3_write_consolidated(store)
        bump_vector_counter(daf, axis, name)
        invisible()
    }
S7::method(format_delete_vector,
           list(ZarrDafReadOnly, S7::class_character, S7::class_character,
                S7::class_logical)) <-
    function(daf, axis, name, must_exist) {
        .zarr_read_only_guard("delete_vector")
    }

# ---- Matrices ------------------------------------------------------------
#
# Layout (mirrors DataAxesFormats.jl/src/zarr_format.jl, Zarr v3):
#   matrices/{rows_axis}/{cols_axis}/{name}/
#       Dense:  zarr.json (array) + c/0/0   (shape = [n_cols, n_rows])
#       Sparse: zarr.json (group) + colptr/, rowval/, [nzval/]
#
# Dense:   upstream stores the array shape REVERSED - [n_cols, n_rows] (there
#          is no `order` field in v3). The on-disk chunk bytes are Julia/R
#          column-major (R's natural matrix layout). A Python zarr reader sees
#          this as a C-contiguous (n_cols, n_rows) array - the transpose of the
#          R/Julia view. Matches upstream exactly.
#
# Sparse:  on-disk colptr / rowval are int64, 1-based (Julia SparseMatrixCSC
#          native); Matrix's @p / @i are 0-based, so we convert with +1L on
#          write, -1L on read. Upstream writes NO attributes for sparse
#          matrices - shape comes from the axis lengths and "all-TRUE Bool" is
#          inferred from the absence of `nzval/`.

S7::method(format_has_matrix,
           list(ZarrDaf, S7::class_character, S7::class_character,
                S7::class_character)) <-
    function(daf, rows_axis, columns_axis, name) {
        .zarr_has_matrix(daf, rows_axis, columns_axis, name)
    }
S7::method(format_has_matrix,
           list(ZarrDafReadOnly, S7::class_character, S7::class_character,
                S7::class_character)) <-
    function(daf, rows_axis, columns_axis, name) {
        .zarr_has_matrix(daf, rows_axis, columns_axis, name)
    }
.zarr_has_matrix <- function(daf, rows_axis, columns_axis, name) {
    store <- S7::prop(daf, "store")
    base <- paste0("matrices/", rows_axis, "/", columns_axis, "/", name)
    # Dense (array) or sparse (group); both carry a zarr.json at `base`.
    !is.null(zarr_v3_read_node(store, base))
}

S7::method(format_matrices_set,
           list(ZarrDaf, S7::class_character, S7::class_character)) <-
    function(daf, rows_axis, columns_axis) {
        .zarr_matrices_set(daf, rows_axis, columns_axis)
    }
S7::method(format_matrices_set,
           list(ZarrDafReadOnly, S7::class_character, S7::class_character)) <-
    function(daf, rows_axis, columns_axis) {
        .zarr_matrices_set(daf, rows_axis, columns_axis)
    }
.zarr_matrices_set <- function(daf, rows_axis, columns_axis) {
    store <- S7::prop(daf, "store")
    prefix <- paste0("matrices/", rows_axis, "/", columns_axis)
    keys <- store_list(store, prefix)
    if (length(keys) == 0L) return(character(0L))
    rel <- sub(paste0("^", prefix, "/"), "", keys)
    # A property (dense array or sparse group) is "<name>/zarr.json" (one
    # slash). Sparse children ("<name>/colptr/zarr.json") are deeper - skipped.
    names <- sub("/zarr.json$", "", rel[grepl("^[^/]+/zarr.json$", rel)])
    sort(unique(names), method = "radix")
}

# Read the matrix at (rows_axis, columns_axis, name). Dispatches on the
# node_type of the single zarr.json: "array" (dense) vs "group" (sparse).
.zarr_get_matrix <- function(daf, rows_axis, columns_axis, name) {
    store <- S7::prop(daf, "store")
    base <- paste0("matrices/", rows_axis, "/", columns_axis, "/", name)
    node <- zarr_v3_read_node(store, base)
    if (is.null(node)) {
        .require_matrix(daf, rows_axis, columns_axis, name, relayout = FALSE)
    }
    if (identical(node$node_type, "array")) {
        return(.zarr_get_dense_matrix(store, base, node))
    }
    nr <- as.integer(format_axis_length(daf, rows_axis))
    nc <- as.integer(format_axis_length(daf, columns_axis))
    .zarr_get_sparse_matrix(store, base, nr, nc)
}

.zarr_get_dense_matrix <- function(store, base, node) {
    if (.zarr_is_sharded(node)) {
        return(.zarr_read_sharded_matrix(store, base, node))
    }
    # Upstream writes shape REVERSED (Julia/R column-major bytes presented to
    # Zarr's C-order metadata as [n_cols, n_rows]). So the *Daf* dimensions are
    # (rows = shape[2], cols = shape[1]).
    on_disk_d0 <- as.integer(node$shape[[1L]])
    on_disk_d1 <- as.integer(node$shape[[2L]])
    nr <- on_disk_d1
    nc <- on_disk_d0
    chunk_path <- zarr_v3_chunk_path(base, 2L)
    total <- nr * nc
    is_string <- identical(node$data_type, "string")
    if (!is_string) {
        mm <- .zarr_try_mmap_dense(store, chunk_path, node$data_type, total, NULL)
        if (!is.null(mm)) {
            # Column-major bytes in (nr, nc) — R's native fill, zero-copy.
            dim(mm) <- c(nr, nc)
            return(mm)
        }
    }
    bytes <- store_get_bytes(store, chunk_path)
    flat <- if (is.null(bytes)) {
        # Zarr omits a chunk that is entirely fill_value (the all-fill
        # optimization); reconstruct it from fill_value rather than erroring
        # "missing chunk" (matching Julia/Zarr.jl). Mirrors the dense-vector path.
        fill <- node$fill_value
        if (is.null(fill)) fill <- if (is_string) "" else 0
        if (is_string) {
            rep(as.character(fill), total)
        } else {
            switch(zarr_v3_r_kind_for_dtype(node$data_type),
                double    = as.double(rep(fill, total)),
                integer   = as.integer(rep(fill, total)),
                integer64 = bit64::as.integer64(rep(fill, total)),
                logical   = as.logical(rep(fill, total)),
                rep(fill, total))
        }
    } else if (is_string) {
        zarr_v3_decode_strings(bytes, n = total)
    } else {
        zarr_v3_decode_chunk(bytes, node$data_type, n = total)
    }
    # The on-disk byte order is column-major in (nr, nc) — R's native fill.
    dim(flat) <- c(nr, nc)
    flat
}

.zarr_get_sparse_matrix <- function(store, base, nr, nc) {
    # colptr (1-based on disk per upstream). DAF writes int64, so decode is
    # integer64 -> as.integer() before the 0-based / dgCMatrix math.
    colptr_meta <- zarr_v3_read_array(store, paste0(base, "/colptr"))
    colptr <- as.integer(.zarr_read_component_vector(
        store, paste0(base, "/colptr"), colptr_meta))
    # rowval (1-based on disk per upstream). Also int64 -> as.integer().
    rowval_meta <- zarr_v3_read_array(store, paste0(base, "/rowval"))
    rowval <- as.integer(.zarr_read_component_vector(
        store, paste0(base, "/rowval"), rowval_meta))
    has_nzval <- !is.null(zarr_v3_read_array(store, paste0(base, "/nzval")))
    if (!has_nzval) {
        # Upstream-compatible: absence of nzval => all-TRUE Bool sparse.
        return(methods::new("lgCMatrix",
            x = rep(TRUE, length(rowval)),
            i = rowval - 1L,
            p = colptr - 1L,
            Dim = c(as.integer(nr), as.integer(nc)),
            Dimnames = list(NULL, NULL)
        ))
    }
    nzval_meta <- zarr_v3_read_array(store, paste0(base, "/nzval"))
    # Non-bool sparse values are read as double below (via as.double() into the
    # dgCMatrix); DAF sparse values are realistically float/bool, but a genuine
    # int64 sparse nzval would narrow to double, lossy above 2^53 - future
    # follow-up if needed.
    nzval <- .zarr_read_component_vector(store, paste0(base, "/nzval"), nzval_meta)
    if (nzval_meta$data_type == "bool") {
        return(methods::new("lgCMatrix",
            x = as.logical(nzval),
            i = rowval - 1L,
            p = colptr - 1L,
            Dim = c(as.integer(nr), as.integer(nc)),
            Dimnames = list(NULL, NULL)
        ))
    }
    methods::new("dgCMatrix",
        x = as.double(nzval),
        i = rowval - 1L,
        p = colptr - 1L,
        Dim = c(as.integer(nr), as.integer(nc)),
        Dimnames = list(NULL, NULL)
    )
}

S7::method(format_get_matrix,
           list(ZarrDaf, S7::class_character, S7::class_character,
                S7::class_character)) <-
    function(daf, rows_axis, columns_axis, name) {
        m <- .attach_matrix_axis_dimnames(daf, rows_axis, columns_axis,
            .zarr_get_matrix(daf, rows_axis, columns_axis, name))
        .cache_group_value(m, .zarr_value_cache_group(m))
    }
S7::method(format_get_matrix,
           list(ZarrDafReadOnly, S7::class_character, S7::class_character,
                S7::class_character)) <-
    function(daf, rows_axis, columns_axis, name) {
        m <- .attach_matrix_axis_dimnames(daf, rows_axis, columns_axis,
            .zarr_get_matrix(daf, rows_axis, columns_axis, name))
        .cache_group_value(m, .zarr_value_cache_group(m))
    }

S7::method(
    format_set_matrix,
    list(ZarrDaf, S7::class_character, S7::class_character,
         S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, rows_axis, columns_axis, name, mat, overwrite) {
    .require_axis(daf, sprintf("for the rows of the matrix: %s", name), rows_axis)
    .require_axis(daf, sprintf("for the columns of the matrix: %s", name), columns_axis)
    if (!overwrite) {
        .require_no_matrix(daf, rows_axis, columns_axis, name, relayout = FALSE)
    }
    nr <- format_axis_length(daf, rows_axis)
    nc <- format_axis_length(daf, columns_axis)
    d <- dim(mat)
    if (is.null(d) || d[[1L]] != nr || d[[2L]] != nc) {
        stop(sprintf(
            "matrix %s has dim %s but axes (%s, %s) require %d x %d",
            sQuote(name),
            paste(d, collapse = " x "),
            sQuote(rows_axis), sQuote(columns_axis),
            nr, nc
        ), call. = FALSE)
    }
    store <- S7::prop(daf, "store")
    base <- paste0("matrices/", rows_axis, "/", columns_axis, "/", name)
    existing <- zarr_v3_read_node(store, base)
    exists_dense <- !is.null(existing) &&
        identical(existing$node_type, "array")
    exists_sparse <- !is.null(existing) &&
        identical(existing$node_type, "group")
    if (exists_dense) {
        store_delete(store, paste0(base, "/zarr.json"))
        store_delete(store, zarr_v3_chunk_path(base, 2L))
    }
    if (exists_sparse) {
        for (k in store_list(store, base)) store_delete(store, k)
    }
    is_sparse <- methods::is(mat, "dgCMatrix") || methods::is(mat, "lgCMatrix")
    if (is_sparse) {
        .zarr_write_sparse_matrix(store, base, mat)
    } else {
        .zarr_write_dense_matrix(store, base, mat, nr, nc)
    }
    bump_matrix_counter(daf, rows_axis, columns_axis, name)
    MEMORY_DATA
}
S7::method(
    format_set_matrix,
    list(ZarrDafReadOnly, S7::class_character, S7::class_character,
         S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, rows_axis, columns_axis, name, mat, overwrite) {
    .zarr_read_only_guard("set_matrix")
}

.zarr_write_dense_matrix <- function(store, base, mat, nr, nc) {
    # On-disk shape is REVERSED [n_cols, n_rows] (matches DAF; no `order`
    # field in v3). Chunk bytes are R/Julia column-major (as.vector flatten).
    dimnames(mat) <- NULL
    .zarr_write_dense_array(store, base, as.vector(mat), c(nc, nr))
    zarr_v3_consolidate_upsert(store, base)
}

.zarr_write_sparse_matrix <- function(store, base, mat) {
    zarr_v3_write_group(store, base)
    is_lg <- methods::is(mat, "lgCMatrix")
    is_all_true_bool <- is_lg && length(mat@x) > 0L &&
                        all(mat@x, na.rm = FALSE)
    # No attributes for sparse matrices - upstream-compatible.
    # colptr + rowval: int64, 1-based (DAF SparseMatrixCSC convention).
    # Matrix's @p / @i are 0-based, so +1L on write.
    colptr_1 <- bit64::as.integer64(mat@p + 1L)
    colptr_base <- paste0(base, "/colptr")
    zarr_v3_write_array(store, colptr_base,
                        zarr_v3_array_meta(shape = length(colptr_1),
                                           dtype = "int64"))
    store_set_bytes(store, zarr_v3_chunk_path(colptr_base, 1L),
                    zarr_v3_encode_chunk(colptr_1, "int64"))
    rowval_1 <- bit64::as.integer64(mat@i + 1L)
    rowval_base <- paste0(base, "/rowval")
    zarr_v3_write_array(store, rowval_base,
                        zarr_v3_array_meta(shape = length(rowval_1),
                                           dtype = "int64"))
    store_set_bytes(store, zarr_v3_chunk_path(rowval_base, 1L),
                    zarr_v3_encode_chunk(rowval_1, "int64"))
    # nzval (skip for all-TRUE Bool — upstream-compatible compaction).
    if (!is_all_true_bool) {
        nzval_dtype <- if (is_lg) "bool" else zarr_v3_dtype_for_r(mat@x)
        nzval_base <- paste0(base, "/nzval")
        zarr_v3_write_array(store, nzval_base,
                            zarr_v3_array_meta(shape = length(mat@x),
                                               dtype = nzval_dtype))
        store_set_bytes(store, zarr_v3_chunk_path(nzval_base, 1L),
                        zarr_v3_encode_chunk(mat@x, nzval_dtype))
    }
    zarr_v3_consolidate_upsert(store, base)
}

S7::method(format_delete_matrix,
           list(ZarrDaf, S7::class_character, S7::class_character,
                S7::class_character, S7::class_logical)) <-
    function(daf, rows_axis, columns_axis, name, must_exist) {
        store <- S7::prop(daf, "store")
        base <- paste0("matrices/", rows_axis, "/", columns_axis, "/", name)
        node <- zarr_v3_read_node(store, base)
        exists_dense <- !is.null(node) && identical(node$node_type, "array")
        exists_sparse <- !is.null(node) && identical(node$node_type, "group")
        if (!exists_dense && !exists_sparse) {
            if (must_exist) {
                .require_matrix(daf, rows_axis, columns_axis, name, relayout = FALSE)
            }
            return(invisible())
        }
        if (exists_dense) {
            store_delete(store, paste0(base, "/zarr.json"))
            store_delete(store, zarr_v3_chunk_path(base, 2L))
        }
        if (exists_sparse) {
            for (k in store_list(store, base)) store_delete(store, k)
        }
        zarr_v3_write_consolidated(store)
        bump_matrix_counter(daf, rows_axis, columns_axis, name)
        invisible()
    }
S7::method(format_delete_matrix,
           list(ZarrDafReadOnly, S7::class_character, S7::class_character,
                S7::class_character, S7::class_logical)) <-
    function(daf, rows_axis, columns_axis, name, must_exist) {
        .zarr_read_only_guard("delete_matrix")
    }

# format_relayout_matrix: read from (rows, cols), write transposed at (cols, rows).
S7::method(format_relayout_matrix,
           list(ZarrDaf, S7::class_character, S7::class_character,
                S7::class_character)) <-
    function(daf, rows_axis, columns_axis, name) {
        m <- .zarr_get_matrix(daf, rows_axis, columns_axis, name)
        if (methods::is(m, "dgCMatrix") || methods::is(m, "lgCMatrix")) {
            transposed <- Matrix::t(m)
        } else {
            transposed <- t(m)
        }
        format_set_matrix(daf, columns_axis, rows_axis, name, transposed,
                          overwrite = TRUE)
        invisible()
    }
S7::method(format_relayout_matrix,
           list(ZarrDafReadOnly, S7::class_character, S7::class_character,
                S7::class_character)) <-
    function(daf, rows_axis, columns_axis, name) {
        .zarr_read_only_guard("relayout_matrix")
    }

# ---- Description header --------------------------------------------------
# Upstream Julia Formats.format_description_header(::ZarrDaf, ...) at
# zarr_format.jl:511 emits type/path/mode. Both writer and read-only
# variants render `type: ZarrDaf`, then path (filesystem dir, ":memory:",
# zip path, or HTTP URL — whichever store_path the constructor recorded)
# and the open mode.
.zarr_daf_description_header <- function(daf, indent) {
    internal <- S7::prop(daf, "internal")
    c(paste0(indent, "type: ZarrDaf"),
      paste0(indent, "path: ", internal$path),
      paste0(indent, "mode: ", internal$mode))
}
S7::method(format_description_header, ZarrDaf) <- function(daf, indent = "",
                                                            deep = FALSE) {
    .zarr_daf_description_header(daf, indent)
}
S7::method(format_description_header, ZarrDafReadOnly) <- function(daf,
                                                                    indent = "",
                                                                    deep = FALSE) {
    .zarr_daf_description_header(daf, indent)
}

# Upstream Julia Readers.is_leaf(::ZarrDaf) at zarr_format.jl:499.
S7::method(.is_leaf_dispatch, ZarrDaf) <- function(daf) TRUE
S7::method(.is_leaf_dispatch, ZarrDafReadOnly) <- function(daf) TRUE

# ---- Reorder ----------------------------------------------------------------
# Best-effort in-place reorder. No on-disk crash recovery yet (the
# files_daf hardlink-backup approach would need a generic store-level
# copy primitive); a crash mid-reorder leaves the store in an undefined
# state. The basic both_axes / single_axis / identity / no_pending
# leaves are exercised; crash_recovery leaves remain skipped.

S7::method(format_replace_reorder, list(ZarrDaf, S7::class_list)) <-
    function(daf, plan, crash_counter = NULL) {
        store <- S7::prop(daf, "store")

        # Axes: rewrite the axis array contents in place.
        for (axis in names(plan$planned_axes)) {
            tick_crash_counter(crash_counter)
            pa <- plan$planned_axes[[axis]]
            base <- paste0("axes/", axis)
            .zarr_write_dense_array(store, base, pa$new_entries,
                                    length(pa$new_entries))
        }

        # Vectors: read, permute, overwrite.
        for (pv in plan$planned_vectors) {
            tick_crash_counter(crash_counter)
            pa <- plan$planned_axes[[pv$axis]]
            v <- format_get_vector(daf, pv$axis, pv$name)$value
            permuted <- if (methods::is(v, "sparseVector")) {
                v[pa$permutation]
            } else {
                v[pa$permutation]
            }
            format_set_vector(daf, pv$axis, pv$name, permuted, overwrite = TRUE)
        }

        # Matrices: read, permute by row / col / both, overwrite.
        for (pm in plan$planned_matrices) {
            tick_crash_counter(crash_counter)
            m <- format_get_matrix(daf, pm$rows_axis, pm$columns_axis,
                                    pm$name)$value
            r_perm <- if (pm$rows_axis %in% names(plan$planned_axes)) {
                plan$planned_axes[[pm$rows_axis]]$permutation
            } else {
                seq_len(nrow(m))
            }
            c_perm <- if (pm$columns_axis %in% names(plan$planned_axes)) {
                plan$planned_axes[[pm$columns_axis]]$permutation
            } else {
                seq_len(ncol(m))
            }
            permuted <- m[r_perm, c_perm, drop = FALSE]
            format_set_matrix(daf, pm$rows_axis, pm$columns_axis,
                              pm$name, permuted, overwrite = TRUE)
        }

        zarr_v3_write_consolidated(store)
        invisible()
    }

S7::method(format_cleanup_reorder, list(ZarrDaf, S7::class_list)) <-
    function(daf, plan, crash_counter = NULL) {
        for (axis in names(plan$planned_axes)) {
            bump_axis_counter(daf, axis)
        }
        for (pv in plan$planned_vectors) {
            bump_vector_counter(daf, pv$axis, pv$name)
        }
        for (pm in plan$planned_matrices) {
            bump_matrix_counter(daf, pm$rows_axis, pm$columns_axis, pm$name)
        }
        invisible()
    }

S7::method(format_reset_reorder, ZarrDaf) <-
    function(daf, crash_counter = NULL) {
        # No backup directory yet; nothing to reset. Returns FALSE so
        # callers see "no pending reorder" - matches Julia's Bool contract.
        invisible(FALSE)
    }
