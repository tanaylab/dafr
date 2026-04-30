#' @include classes.R zarr_store.R zarr_v2.R cache.R cache_group.R format_api.R utils.R
NULL

#' Zarr-backed Daf class.
#'
#' Concrete `DafWriter` subclass instantiated by [zarr_daf()] when
#' opened with mode `"w"` / `"w+"` / `"r+"`. Operates against any
#' [ZarrStore]; today: `DirStore` (filesystem) and `DictStore`
#' (in-memory). Zip-backed Zarr (`MmapZipStore`) lands in slice 17.
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
#' `:memory:` or `NULL` creates a `DictStore`. `*.daf.zarr.zip` errors
#' with "lands in slice 17" pending the C++ MmapZipStore.
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
    if (is_memory) {
        store <- new_dict_store()
        store_path <- ":memory:"
    } else if (grepl("\\.daf\\.zarr\\.zip(#.*)?$", uri)) {
        new_mmap_zip_store(uri) # errors out
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

    if (mode %in% c("w", "w+") && !store_exists(store, "daf.json")) {
        .zarr_daf_init_store(store)
    }

    if (mode %in% c("r", "r+") && !store_exists(store, "daf.json")) {
        stop(sprintf(
            "zarr_daf: store at %s missing daf.json; not a valid ZarrDaf store",
            sQuote(store_path)
        ), call. = FALSE)
    }

    final_name <- if (!is.null(name)) name else basename(store_path)
    cls <- if (mode == "r") ZarrDafReadOnly else ZarrDaf
    cls(
        name                   = final_name,
        store                  = store,
        internal               = new_internal_env(),
        cache                  = new_cache_env(),
        axis_version_counter   = new_counter_env(),
        vector_version_counter = new_counter_env(),
        matrix_version_counter = new_counter_env()
    )
}

# Initialize an empty Zarr store with the daf.json marker. Also writes
# the root `.zgroup` (so zarr-python v3's `zarr.open()` recognises the
# directory as a Zarr v2 group) and an empty `.zmetadata` (consolidated
# metadata, see zarr_v2.R) so both files exist from store creation and
# the invariant "every ZarrDaf store has `.zgroup` + `.zmetadata` at
# its root" holds unconditionally.
.zarr_daf_init_store <- function(store) {
    daf_meta <- list(version = "0.2.0", format = "zarr_daf")
    store_set_bytes(
        store, "daf.json",
        charToRaw(jsonlite::toJSON(daf_meta, auto_unbox = TRUE))
    )
    store_set_bytes(store, ".zgroup", .ZARR_ZGROUP_BYTES)
    zarr_v2_write_zmetadata(store)
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
        store_exists(
            S7::prop(daf, "store"),
            paste0("scalars/", name, "/.zarray")
        )
    }
S7::method(format_has_scalar, list(ZarrDafReadOnly, S7::class_character)) <-
    function(daf, name) {
        store_exists(
            S7::prop(daf, "store"),
            paste0("scalars/", name, "/.zarray")
        )
    }

.zarr_get_scalar <- function(daf, name) {
    store <- S7::prop(daf, "store")
    path <- paste0("scalars/", name)
    zarray <- zarr_v2_read_zarray(store, path)
    if (is.null(zarray)) {
        .require_scalar(daf, name)
    }
    chunk_path <- paste0(path, "/0")
    bytes <- store_get_bytes(store, chunk_path)
    if (is.null(bytes)) {
        stop(sprintf("scalar %s missing chunk", sQuote(name)), call. = FALSE)
    }
    if (zarray$dtype == "|O") {
        decoded <- zarr_v2_decode_strings(bytes, n = 1L)
        return(decoded[[1L]])
    }
    decoded <- zarr_v2_decode_chunk(bytes, zarray$dtype,
        n = 1L,
        compressor = zarray$compressor
    )
    decoded[[1L]]
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
    suffix <- "/.zarray"
    matched <- keys[endsWith(keys, suffix)]
    names_only <- sub("^scalars/", "", sub(paste0(suffix, "$"), "", matched))
    sort(names_only, method = "radix")
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

.zarr_write_scalar <- function(store, name, value) {
    path <- paste0("scalars/", name)
    dtype <- zarr_v2_dtype_for_r(value)
    zarray <- zarr_v2_zarray(shape = 1L, dtype = dtype)
    if (dtype == "|O") {
        zarray$filters <- list(list(id = "vlen-utf8"))
        chunk_bytes <- zarr_v2_encode_strings(value)
    } else {
        chunk_bytes <- zarr_v2_encode_chunk(value, dtype)
    }
    zarr_v2_write_zarray(store, path, zarray)
    store_set_bytes(store, paste0(path, "/0"), chunk_bytes)
    zarr_v2_write_zmetadata(store)
}

S7::method(
    format_delete_scalar,
    list(ZarrDaf, S7::class_character, S7::class_logical)
) <- function(daf, name, must_exist) {
    store <- S7::prop(daf, "store")
    if (!store_exists(store, paste0("scalars/", name, "/.zarray"))) {
        if (must_exist) {
            .require_scalar(daf, name)
        }
        return(invisible())
    }
    store_delete(store, paste0("scalars/", name, "/.zarray"))
    store_delete(store, paste0("scalars/", name, "/0"))
    zarr_v2_write_zmetadata(store)
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
        store_exists(
            S7::prop(daf, "store"),
            paste0("axes/", axis, "/.zarray")
        )
    }
S7::method(format_has_axis, list(ZarrDafReadOnly, S7::class_character)) <-
    function(daf, axis) {
        store_exists(
            S7::prop(daf, "store"),
            paste0("axes/", axis, "/.zarray")
        )
    }

S7::method(format_axes_set, ZarrDaf) <- function(daf) .zarr_axes_set(daf)
S7::method(format_axes_set, ZarrDafReadOnly) <- function(daf) .zarr_axes_set(daf)
.zarr_axes_set <- function(daf) {
    store <- S7::prop(daf, "store")
    keys <- store_list(store, "axes")
    suffix <- "/.zarray"
    matched <- keys[endsWith(keys, suffix)]
    names_only <- sub("^axes/", "", sub(paste0(suffix, "$"), "", matched))
    sort(names_only, method = "radix")
}

.zarr_axis_entries <- function(daf, axis) {
    store <- S7::prop(daf, "store")
    path <- paste0("axes/", axis)
    zarray <- zarr_v2_read_zarray(store, path)
    if (is.null(zarray)) {
        .require_axis(daf, "for: zarr backend", axis)
    }
    chunk_bytes <- store_get_bytes(store, paste0(path, "/0"))
    if (is.null(chunk_bytes)) {
        stop(sprintf("axis %s missing chunk", sQuote(axis)), call. = FALSE)
    }
    n <- as.integer(zarray$shape[[1L]])
    zarr_v2_decode_strings(chunk_bytes, n = n)
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
    path <- paste0("axes/", axis)
    n <- length(entries)
    zarray <- zarr_v2_zarray(shape = n, dtype = "|O")
    zarray$filters <- list(list(id = "vlen-utf8"))
    zarr_v2_write_zarray(store, path, zarray)
    store_set_bytes(
        store, paste0(path, "/0"),
        zarr_v2_encode_strings(entries)
    )
    zarr_v2_write_zmetadata(store)
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
    if (!store_exists(store, paste0("axes/", axis, "/.zarray"))) {
        if (must_exist) {
            .require_axis(daf, "for: delete_axis", axis)
        }
        return(invisible())
    }
    store_delete(store, paste0("axes/", axis, "/.zarray"))
    store_delete(store, paste0("axes/", axis, "/0"))
    zarr_v2_write_zmetadata(store)
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

# Group marker for sparse layouts (Zarr v2 .zgroup convention).
.ZARR_ZGROUP_BYTES <- charToRaw('{"zarr_format":2}')

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
    base <- paste0("vectors/", axis, "/", name)
    # Either dense (.zarray at base) or sparse (.zgroup at base).
    store_exists(store, paste0(base, "/.zarray")) ||
        store_exists(store, paste0(base, "/.zgroup"))
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
    # Strip the "vectors/{axis}/" prefix and split remainder on "/".
    rel <- sub(paste0("^", prefix, "/"), "", keys)
    # Top-level dense:   "{name}/.zarray"          (one slash, ends .zarray)
    # Top-level sparse:  "{name}/.zgroup"          (one slash, ends .zgroup)
    # Sparse children:   "{name}/nzind/.zarray"    (multiple slashes — skip)
    is_dense <- grepl("^[^/]+/\\.zarray$", rel)
    is_sparse <- grepl("^[^/]+/\\.zgroup$", rel)
    names <- c(
        sub("/\\.zarray$", "", rel[is_dense]),
        sub("/\\.zgroup$", "", rel[is_sparse])
    )
    sort(unique(names), method = "radix")
}

# format_get_vector dispatches based on dense vs sparse layout on disk.
.zarr_get_vector <- function(daf, axis, name) {
    store <- S7::prop(daf, "store")
    base <- paste0("vectors/", axis, "/", name)
    if (store_exists(store, paste0(base, "/.zarray"))) {
        # Dense path
        zarray <- zarr_v2_read_zarray(store, base)
        n <- as.integer(zarray$shape[[1L]])
        chunk <- store_get_bytes(store, paste0(base, "/0"))
        if (is.null(chunk)) {
            stop(sprintf("vector %s missing chunk", sQuote(name)),
                 call. = FALSE)
        }
        if (zarray$dtype == "|O") {
            return(zarr_v2_decode_strings(chunk, n = n))
        }
        return(zarr_v2_decode_chunk(chunk, zarray$dtype, n = n,
                                    compressor = zarray$compressor))
    }
    if (store_exists(store, paste0(base, "/.zgroup"))) {
        return(.zarr_get_sparse_vector(daf, axis, name))
    }
    .require_vector(daf, axis, name)
}

.zarr_get_sparse_vector <- function(daf, axis, name) {
    # Returns a DENSE atomic vector to match the existing dafr convention
    # (FilesDaf densifies sparse vectors at the read boundary too — the
    # user-facing get_vector contract is "named atomic vector"). On-disk
    # layout is still sparse; densification happens on read.
    #
    # Upstream parity (matches sparse-matrix layout):
    #   - No `.zattrs` is written. The vector's full length comes from
    #     the axis length, not from a stored `n`.
    #   - The all-TRUE Bool case is inferred from the ABSENCE of
    #     `nzval/.zarray` in the store.
    store <- S7::prop(daf, "store")
    base <- paste0("vectors/", axis, "/", name)
    n <- as.integer(format_axis_length(daf, axis))
    nzind_zarray <- zarr_v2_read_zarray(store, paste0(base, "/nzind"))
    nzind_n <- as.integer(nzind_zarray$shape[[1L]])
    nzind <- zarr_v2_decode_chunk(
        store_get_bytes(store, paste0(base, "/nzind/0")),
        nzind_zarray$dtype, n = nzind_n,
        compressor = nzind_zarray$compressor
    )
    # Upstream stores 1-based indices on disk (Julia SparseVector convention).
    nzind_1 <- as.integer(nzind)

    has_nzval <- store_exists(store, paste0(base, "/nzval/.zarray"))
    if (!has_nzval) {
        # All-TRUE Bool sparse: nzval was omitted on write; synthesize.
        out <- logical(n)
        out[nzind_1] <- TRUE
        return(out)
    }
    nzval_zarray <- zarr_v2_read_zarray(store, paste0(base, "/nzval"))
    nzval_n <- as.integer(nzval_zarray$shape[[1L]])
    nzval <- zarr_v2_decode_chunk(
        store_get_bytes(store, paste0(base, "/nzval/0")),
        nzval_zarray$dtype, n = nzval_n,
        compressor = nzval_zarray$compressor
    )
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
        .cache_group_value(.zarr_get_vector(daf, axis, name), MEMORY_DATA)
    }
S7::method(format_get_vector,
           list(ZarrDafReadOnly, S7::class_character, S7::class_character)) <-
    function(daf, axis, name) {
        .cache_group_value(.zarr_get_vector(daf, axis, name), MEMORY_DATA)
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
    exists_dense <- store_exists(store, paste0(base, "/.zarray"))
    exists_sparse <- store_exists(store, paste0(base, "/.zgroup"))
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
        store_delete(store, paste0(base, "/.zarray"))
        store_delete(store, paste0(base, "/0"))
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
    dtype <- zarr_v2_dtype_for_r(vec)
    zarray <- zarr_v2_zarray(shape = length(vec), dtype = dtype)
    if (dtype == "|O") {
        zarray$filters <- list(list(id = "vlen-utf8"))
        chunk <- zarr_v2_encode_strings(vec)
    } else {
        chunk <- zarr_v2_encode_chunk(vec, dtype)
    }
    zarr_v2_write_zarray(store, base, zarray)
    store_set_bytes(store, paste0(base, "/0"), chunk)
    zarr_v2_write_zmetadata(store)
}

.zarr_write_sparse_vector <- function(store, base, vec) {
    # Mark as group. NO .zattrs is written — upstream parity with
    # sparse matrices: shape comes from the axis length on read, and
    # the all-TRUE Bool case is inferred from absence of `nzval/`.
    store_set_bytes(store, paste0(base, "/.zgroup"), .ZARR_ZGROUP_BYTES)
    is_all_true_bool <- is.logical(vec@x) && length(vec@x) > 0L &&
                        all(vec@x, na.rm = FALSE)
    # Write nzind: 1-based indices (upstream Julia SparseVector convention).
    nzind <- as.integer(vec@i)
    nzind_dtype <- "<i4"  # int32; upgrade to <i8 if/when we hit huge axes.
    nzind_zarray <- zarr_v2_zarray(shape = length(nzind), dtype = nzind_dtype)
    zarr_v2_write_zarray(store, paste0(base, "/nzind"), nzind_zarray)
    store_set_bytes(store, paste0(base, "/nzind/0"),
                    zarr_v2_encode_chunk(nzind, nzind_dtype))
    # Write nzval (skip for all-TRUE Bool — upstream-compatible compaction).
    if (!is_all_true_bool) {
        nzval_dtype <- zarr_v2_dtype_for_r(vec@x)
        nzval_zarray <- zarr_v2_zarray(shape = length(vec@x),
                                       dtype = nzval_dtype)
        zarr_v2_write_zarray(store, paste0(base, "/nzval"), nzval_zarray)
        store_set_bytes(store, paste0(base, "/nzval/0"),
                        zarr_v2_encode_chunk(vec@x, nzval_dtype))
    }
    zarr_v2_write_zmetadata(store)
}

S7::method(format_delete_vector,
           list(ZarrDaf, S7::class_character, S7::class_character,
                S7::class_logical)) <-
    function(daf, axis, name, must_exist) {
        store <- S7::prop(daf, "store")
        base <- paste0("vectors/", axis, "/", name)
        exists_dense <- store_exists(store, paste0(base, "/.zarray"))
        exists_sparse <- store_exists(store, paste0(base, "/.zgroup"))
        if (!exists_dense && !exists_sparse) {
            if (must_exist) {
                .require_vector(daf, axis, name)
            }
            return(invisible())
        }
        if (exists_dense) {
            store_delete(store, paste0(base, "/.zarray"))
            store_delete(store, paste0(base, "/0"))
        }
        if (exists_sparse) {
            for (k in store_list(store, base)) store_delete(store, k)
        }
        zarr_v2_write_zmetadata(store)
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
# Layout (mirrors DataAxesFormats.jl/src/zarr_format.jl):
#   matrices/{rows_axis}/{cols_axis}/{name}/
#       Dense:  .zarray + 0/0   (shape = [n_cols, n_rows], order = "C")
#       Sparse: .zgroup + colptr/, rowval/, [nzval/]
#
# Dense:   upstream stores `.zarray` shape REVERSED — [n_cols, n_rows] with
#          order = "C". The on-disk bytes are Julia/R column-major (R's
#          natural matrix layout). A Python zarr reader sees this as a
#          C-contiguous (n_cols, n_rows) array — the transpose of the
#          R/Julia view. Matches upstream exactly.
#
# Sparse:  on-disk colptr / rowval are 1-based (Julia SparseMatrixCSC native);
#          Matrix's @p / @i are 0-based, so we convert with +1L on write,
#          -1L on read. Upstream writes NO .zattrs for sparse matrices —
#          shape comes from the axis lengths and "all-TRUE Bool" is inferred
#          from the absence of `nzval/`.

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
    store_exists(store, paste0(base, "/.zarray")) ||
        store_exists(store, paste0(base, "/.zgroup"))
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
    # Top-level dense:  "{name}/.zarray"  (one slash, ends .zarray)
    # Top-level sparse: "{name}/.zgroup"  (one slash, ends .zgroup)
    # Sparse children:  "{name}/colptr/.zarray" — multiple slashes, skip.
    is_dense <- grepl("^[^/]+/\\.zarray$", rel)
    is_sparse <- grepl("^[^/]+/\\.zgroup$", rel)
    names <- c(
        sub("/\\.zarray$", "", rel[is_dense]),
        sub("/\\.zgroup$", "", rel[is_sparse])
    )
    sort(unique(names), method = "radix")
}

# Read the matrix at (rows_axis, columns_axis, name). Dispatches on
# .zarray (dense) vs .zgroup (sparse).
.zarr_get_matrix <- function(daf, rows_axis, columns_axis, name) {
    store <- S7::prop(daf, "store")
    base <- paste0("matrices/", rows_axis, "/", columns_axis, "/", name)
    if (store_exists(store, paste0(base, "/.zarray"))) {
        return(.zarr_get_dense_matrix(store, base))
    }
    if (store_exists(store, paste0(base, "/.zgroup"))) {
        nr <- as.integer(format_axis_length(daf, rows_axis))
        nc <- as.integer(format_axis_length(daf, columns_axis))
        return(.zarr_get_sparse_matrix(store, base, nr, nc))
    }
    .require_matrix(daf, rows_axis, columns_axis, name, relayout = FALSE)
}

.zarr_get_dense_matrix <- function(store, base) {
    zarray <- zarr_v2_read_zarray(store, base)
    # Upstream writes .zarray shape REVERSED (Julia/R column-major bytes
    # presented to Zarr's C-order metadata as [n_cols, n_rows]). So the
    # *Daf* dimensions are (rows = shape[2], cols = shape[1]).
    on_disk_d0 <- as.integer(zarray$shape[[1L]])
    on_disk_d1 <- as.integer(zarray$shape[[2L]])
    nr <- on_disk_d1
    nc <- on_disk_d0
    chunk_path <- paste0(base, "/0/0")
    bytes <- store_get_bytes(store, chunk_path)
    if (is.null(bytes)) {
        stop(sprintf("matrix at %s missing chunk", sQuote(base)), call. = FALSE)
    }
    if (zarray$dtype == "|O") {
        flat <- zarr_v2_decode_strings(bytes, n = nr * nc)
    } else {
        flat <- zarr_v2_decode_chunk(bytes, zarray$dtype, n = nr * nc,
                                     compressor = zarray$compressor)
    }
    # The on-disk byte order is column-major in (nr, nc) — R's native fill.
    dim(flat) <- c(nr, nc)
    flat
}

.zarr_get_sparse_matrix <- function(store, base, nr, nc) {
    # colptr (1-based on disk per upstream).
    colptr_zarray <- zarr_v2_read_zarray(store, paste0(base, "/colptr"))
    colptr <- zarr_v2_decode_chunk(
        store_get_bytes(store, paste0(base, "/colptr/0")),
        colptr_zarray$dtype,
        n = as.integer(colptr_zarray$shape[[1L]]),
        compressor = colptr_zarray$compressor
    )
    # rowval (1-based on disk per upstream).
    rowval_zarray <- zarr_v2_read_zarray(store, paste0(base, "/rowval"))
    rowval_n <- as.integer(rowval_zarray$shape[[1L]])
    rowval <- if (rowval_n == 0L) integer(0L) else
        zarr_v2_decode_chunk(
            store_get_bytes(store, paste0(base, "/rowval/0")),
            rowval_zarray$dtype, n = rowval_n,
            compressor = rowval_zarray$compressor
        )
    has_nzval <- store_exists(store, paste0(base, "/nzval/.zarray"))
    if (!has_nzval) {
        # Upstream-compatible: absence of nzval => all-TRUE Bool sparse.
        return(methods::new("lgCMatrix",
            x = rep(TRUE, length(rowval)),
            i = as.integer(rowval) - 1L,
            p = as.integer(colptr) - 1L,
            Dim = c(as.integer(nr), as.integer(nc)),
            Dimnames = list(NULL, NULL)
        ))
    }
    nzval_zarray <- zarr_v2_read_zarray(store, paste0(base, "/nzval"))
    nzval_n <- as.integer(nzval_zarray$shape[[1L]])
    nzval <- if (nzval_n == 0L) {
        if (nzval_zarray$dtype == "|b1") logical(0L) else double(0L)
    } else {
        zarr_v2_decode_chunk(
            store_get_bytes(store, paste0(base, "/nzval/0")),
            nzval_zarray$dtype, n = nzval_n,
            compressor = nzval_zarray$compressor
        )
    }
    if (nzval_zarray$dtype == "|b1") {
        return(methods::new("lgCMatrix",
            x = as.logical(nzval),
            i = as.integer(rowval) - 1L,
            p = as.integer(colptr) - 1L,
            Dim = c(as.integer(nr), as.integer(nc)),
            Dimnames = list(NULL, NULL)
        ))
    }
    methods::new("dgCMatrix",
        x = as.double(nzval),
        i = as.integer(rowval) - 1L,
        p = as.integer(colptr) - 1L,
        Dim = c(as.integer(nr), as.integer(nc)),
        Dimnames = list(NULL, NULL)
    )
}

S7::method(format_get_matrix,
           list(ZarrDaf, S7::class_character, S7::class_character,
                S7::class_character)) <-
    function(daf, rows_axis, columns_axis, name) {
        .cache_group_value(
            .zarr_get_matrix(daf, rows_axis, columns_axis, name),
            MEMORY_DATA
        )
    }
S7::method(format_get_matrix,
           list(ZarrDafReadOnly, S7::class_character, S7::class_character,
                S7::class_character)) <-
    function(daf, rows_axis, columns_axis, name) {
        .cache_group_value(
            .zarr_get_matrix(daf, rows_axis, columns_axis, name),
            MEMORY_DATA
        )
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
    exists_dense <- store_exists(store, paste0(base, "/.zarray"))
    exists_sparse <- store_exists(store, paste0(base, "/.zgroup"))
    if (exists_dense) {
        store_delete(store, paste0(base, "/.zarray"))
        store_delete(store, paste0(base, "/0/0"))
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
    dimnames(mat) <- NULL
    flat <- as.vector(mat)  # R column-major flatten — bytes match Julia.
    dtype <- zarr_v2_dtype_for_r(flat)
    # Shape on disk is REVERSED [n_cols, n_rows]; order = "C". Matches upstream.
    zarray <- zarr_v2_zarray(shape = c(nc, nr), dtype = dtype, order = "C")
    if (dtype == "|O") {
        zarray$filters <- list(list(id = "vlen-utf8"))
        chunk <- zarr_v2_encode_strings(flat)
    } else {
        chunk <- zarr_v2_encode_chunk(flat, dtype)
    }
    zarr_v2_write_zarray(store, base, zarray)
    store_set_bytes(store, paste0(base, "/0/0"), chunk)
    zarr_v2_write_zmetadata(store)
}

.zarr_write_sparse_matrix <- function(store, base, mat) {
    store_set_bytes(store, paste0(base, "/.zgroup"), .ZARR_ZGROUP_BYTES)
    is_lg <- methods::is(mat, "lgCMatrix")
    is_all_true_bool <- is_lg && length(mat@x) > 0L &&
                        all(mat@x, na.rm = FALSE)
    # No .zattrs for sparse matrices — upstream-compatible.
    # colptr (Matrix's @p is 0-based; on disk we want 1-based per upstream).
    colptr_1 <- as.integer(mat@p) + 1L
    colptr_dtype <- "<i4"
    colptr_zarray <- zarr_v2_zarray(shape = length(colptr_1),
                                    dtype = colptr_dtype)
    zarr_v2_write_zarray(store, paste0(base, "/colptr"), colptr_zarray)
    store_set_bytes(store, paste0(base, "/colptr/0"),
                    zarr_v2_encode_chunk(colptr_1, colptr_dtype))
    # rowval (Matrix's @i is 0-based; on disk 1-based).
    rowval_1 <- as.integer(mat@i) + 1L
    rowval_dtype <- "<i4"
    rowval_zarray <- zarr_v2_zarray(shape = length(rowval_1),
                                    dtype = rowval_dtype)
    zarr_v2_write_zarray(store, paste0(base, "/rowval"), rowval_zarray)
    store_set_bytes(store, paste0(base, "/rowval/0"),
                    zarr_v2_encode_chunk(rowval_1, rowval_dtype))
    # nzval (skip for all-TRUE Bool — upstream-compatible compaction).
    if (!is_all_true_bool) {
        nzval_dtype <- if (is_lg) "|b1" else zarr_v2_dtype_for_r(mat@x)
        nzval_zarray <- zarr_v2_zarray(shape = length(mat@x),
                                       dtype = nzval_dtype)
        zarr_v2_write_zarray(store, paste0(base, "/nzval"), nzval_zarray)
        store_set_bytes(store, paste0(base, "/nzval/0"),
                        zarr_v2_encode_chunk(mat@x, nzval_dtype))
    }
    zarr_v2_write_zmetadata(store)
}

S7::method(format_delete_matrix,
           list(ZarrDaf, S7::class_character, S7::class_character,
                S7::class_character, S7::class_logical)) <-
    function(daf, rows_axis, columns_axis, name, must_exist) {
        store <- S7::prop(daf, "store")
        base <- paste0("matrices/", rows_axis, "/", columns_axis, "/", name)
        exists_dense <- store_exists(store, paste0(base, "/.zarray"))
        exists_sparse <- store_exists(store, paste0(base, "/.zgroup"))
        if (!exists_dense && !exists_sparse) {
            if (must_exist) {
                .require_matrix(daf, rows_axis, columns_axis, name, relayout = FALSE)
            }
            return(invisible())
        }
        if (exists_dense) {
            store_delete(store, paste0(base, "/.zarray"))
            store_delete(store, paste0(base, "/0/0"))
        }
        if (exists_sparse) {
            for (k in store_list(store, base)) store_delete(store, k)
        }
        zarr_v2_write_zmetadata(store)
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
