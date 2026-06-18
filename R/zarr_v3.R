# R/zarr_v3.R
# Zarr v3 metadata + chunk I/O layer. Operates against any ZarrStore from
# R/zarr_store.R; does no I/O of its own beyond the Store interface.
# Replaces the deleted R/zarr_v2.R. On-disk format matches DataAxesFormats.jl
# 0.3.0 (Zarr v3): single zarr.json per node, lowercase dtype names, c/-prefixed
# chunk keys, daf marker as a root-group attribute, inline consolidated metadata.

#' @importFrom methods as
NULL

# ---- dtype mapping (R type <-> Zarr v3 data_type name) -------------------

# Zarr v3 data_type string for an R value. Always little-endian.
zarr_v3_dtype_for_r <- function(value) {
    if (is.character(value)) return("string")
    if (is.logical(value)) return("bool")
    if (inherits(value, "integer64")) return("int64")
    if (is.integer(value)) return("int32")
    if (is.numeric(value)) return("float64")
    stop(sprintf("zarr_v3: cannot map R type %s to a Zarr v3 data_type",
                 class(value)[1L]), call. = FALSE)
}

# R "kind" (readBin target) for a Zarr v3 data_type name.
zarr_v3_r_kind_for_dtype <- function(dtype) {
    switch(dtype,
        "float64" = "double", "float32" = "double",
        "int8" = "integer", "int16" = "integer", "int32" = "integer",
        "uint8" = "integer", "uint16" = "integer", "uint32" = "integer",
        "int64" = "integer64", "uint64" = "integer64",
        "bool" = "logical",
        "string" = "character",
        stop(sprintf("zarr_v3: unsupported data_type %s", sQuote(dtype)),
             call. = FALSE))
}

# Element byte size for a fixed-width v3 dtype; NA for variable-length (string).
zarr_v3_size_for_dtype <- function(dtype) {
    switch(dtype,
        "float64" = 8L, "float32" = 4L,
        "int64" = 8L, "uint64" = 8L,
        "int32" = 4L, "uint32" = 4L,
        "int16" = 2L, "uint16" = 2L,
        "int8" = 1L, "uint8" = 1L, "bool" = 1L,
        "string" = NA_integer_,
        stop(sprintf("zarr_v3: unsupported data_type %s", sQuote(dtype)),
             call. = FALSE))
}

# ---- chunk keys (v3 default encoding: prefix "c", separator "/") ---------

# Single-chunk key for an ndim-dimensional array: "c/0" (1-D), "c/0/0" (2-D).
zarr_v3_chunk_key <- function(ndim) {
    paste(c("c", rep("0", ndim)), collapse = "/")
}

# Full store key for the single chunk of the array at `base`.
zarr_v3_chunk_path <- function(base, ndim) {
    paste0(base, "/", zarr_v3_chunk_key(ndim))
}

# ---- array zarr.json -----------------------------------------------------

# Build a Zarr v3 array metadata list (one chunk == shape). `bytes` codec for
# numeric dtypes (little-endian); `vlen-utf8` for strings. fill_value defaults
# to the dtype zero ("" for strings).
zarr_v3_array_meta <- function(shape, dtype, fill_value = NULL) {
    shape <- as.integer(shape)
    if (is.null(fill_value)) {
        fill_value <- if (dtype == "string") "" else
            if (dtype == "bool") FALSE else
            if (zarr_v3_r_kind_for_dtype(dtype) == "double") 0.0 else 0L
    }
    codecs <- if (dtype == "string") {
        list(list(name = "vlen-utf8", configuration = structure(list(),
                                                  names = character(0L))))
    } else {
        list(list(name = "bytes", configuration = list(endian = "little")))
    }
    list(
        zarr_format = 3L,
        node_type = "array",
        shape = as.list(shape),
        data_type = dtype,
        chunk_grid = list(name = "regular",
                          configuration = list(chunk_shape = as.list(shape))),
        chunk_key_encoding = list(name = "default",
                                  configuration = list(separator = "/")),
        codecs = codecs,
        fill_value = fill_value,
        attributes = structure(list(), names = character(0L))
    )
}

# Write path/zarr.json for an array, ensuring every ancestor group marker.
zarr_v3_write_array <- function(store, path, meta) {
    .zarr_v3_ensure_ancestor_groups(store, path)
    json <- jsonlite::toJSON(meta, auto_unbox = TRUE, null = "null",
                             pretty = FALSE)
    store_set_bytes(store, paste0(path, "/zarr.json"),
                    charToRaw(as.character(json)))
    invisible()
}

# Read + parse a node's zarr.json (array or group), or NULL if absent.
zarr_v3_read_node <- function(store, path) {
    key <- if (nzchar(path)) paste0(path, "/zarr.json") else "zarr.json"
    raw <- store_get_bytes(store, key)
    if (is.null(raw)) return(NULL)
    jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
}

# Read an array node; NULL if absent or not an array.
zarr_v3_read_array <- function(store, path) {
    node <- zarr_v3_read_node(store, path)
    if (is.null(node) || !identical(node$node_type, "array")) return(NULL)
    node
}

# ---- group zarr.json -----------------------------------------------------

# Group metadata for a plain (non-root) group.
.ZARR_V3_GROUP <- list(zarr_format = 3L, node_type = "group")

# Write a plain group marker at `path` (idempotent on append-only stores).
zarr_v3_write_group <- function(store, path) {
    key <- if (nzchar(path)) paste0(path, "/zarr.json") else "zarr.json"
    if (store_exists(store, key)) return(invisible())
    json <- jsonlite::toJSON(.ZARR_V3_GROUP, auto_unbox = TRUE, pretty = FALSE)
    store_set_bytes(store, key, charToRaw(as.character(json)))
    invisible()
}

# Ensure every ancestor group of an array `path` carries a group zarr.json.
# Mirrors DAF's directory layout (every intermediate dir is a v3 group).
.zarr_v3_ensure_ancestor_groups <- function(store, path) {
    parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
    if (length(parts) <= 1L) return(invisible())
    for (i in seq_len(length(parts) - 1L)) {
        zarr_v3_write_group(store, paste(parts[seq_len(i)], collapse = "/"))
    }
    invisible()
}

# ---- root marker + consolidated metadata ---------------------------------

# Daf-zarr format version (root-group attribute `daf: [MAJOR, MINOR]`).
.ZARR_V3_DAF_MAJOR <- 1L
.ZARR_V3_DAF_MINOR <- 0L

# Write the root group zarr.json with the daf version attribute. Consolidated
# metadata is (re)written separately via zarr_v3_write_consolidated().
zarr_v3_write_root <- function(store) {
    root <- list(
        zarr_format = 3L,
        node_type = "group",
        attributes = list(daf = list(.ZARR_V3_DAF_MAJOR, .ZARR_V3_DAF_MINOR))
    )
    json <- jsonlite::toJSON(root, auto_unbox = TRUE, pretty = FALSE)
    store_set_bytes(store, "zarr.json", charToRaw(as.character(json)))
    invisible()
}

# TRUE if the store's root zarr.json is a group carrying a `daf` attribute.
zarr_v3_daf_marker_exists <- function(store) {
    node <- zarr_v3_read_node(store, "")
    !is.null(node) && identical(node$node_type, "group") &&
        !is.null(node$attributes) && !is.null(node$attributes$daf)
}

# The [MAJOR, MINOR] version from the root daf attribute (integer vector).
zarr_v3_daf_version <- function(store) {
    node <- zarr_v3_read_node(store, "")
    as.integer(unlist(node$attributes$daf))
}

# The store's in-memory consolidate cache env (holds `$cjson`, a path -> node
# zarr.json string map), or NULL for stores that don't carry one. Only
# DirStore/DictStore consolidate incrementally; zip no-ops and http is read-only.
.zarr_v3_cache <- function(store) {
    if (S7::S7_inherits(store, DirStore) || S7::S7_inherits(store, DictStore)) {
        return(S7::prop(store, "consolidate_cache"))
    }
    NULL
}

# Minimal JSON-string escape for an object key (node path). daf paths are simple
# ASCII, but guard backslash + double-quote for arbitrary axis/property names.
.zarr_v3_escape_key <- function(s) {
    s <- gsub("\\", "\\\\", s, fixed = TRUE)
    gsub('"', '\\"', s, fixed = TRUE)
}

# Assemble the root group zarr.json from a path -> node-JSON-string map. A
# non-root node's on-disk zarr.json IS exactly the text to inline (nodes never
# carry their own consolidated_metadata), so the root is built by string
# concatenation - no per-node parse / re-serialize, which is what made the full
# rebuild O(N^2). Output is valid JSON, ordered by path; equivalent after a parse
# to a full re-serialize.
.zarr_v3_assemble_root <- function(node_json) {
    paths <- sort(names(node_json), method = "radix")
    body <- if (length(paths) == 0L) "" else paste0(
        '"', .zarr_v3_escape_key(paths), '":',
        unlist(node_json[paths], use.names = FALSE), collapse = ",")
    tmpl <- as.character(jsonlite::toJSON(list(
        zarr_format = 3L, node_type = "group",
        attributes = list(daf = list(.ZARR_V3_DAF_MAJOR, .ZARR_V3_DAF_MINOR)),
        consolidated_metadata = list(kind = "inline", must_understand = FALSE,
                                     metadata = "__DAFR_MD__")),
        auto_unbox = TRUE))
    sub('"__DAFR_MD__"', paste0("{", body, "}"), tmpl, fixed = TRUE)
}

# Write the root group zarr.json from a path -> node-JSON-string map.
.zarr_v3_write_root_json <- function(store, node_json) {
    store_set_bytes(store, "zarr.json",
                    charToRaw(.zarr_v3_assemble_root(node_json)))
    invisible()
}

# Full rebuild of the root consolidated metadata: index every non-root node by
# relative path -> its raw zarr.json string. O(N) per call -> O(N^2) over a bulk
# write, so mutations call zarr_v3_consolidate_upsert() instead; this full scan
# is kept for store init, deletes (which can cascade), reorder, and as the
# incremental fallback. Refreshes the in-memory cache so a following upsert is in
# sync. No-op on append-only zip stores (DAF omits consolidated metadata there
# and uses the zip central directory).
zarr_v3_write_consolidated <- function(store) {
    if (S7::S7_inherits(store, MmapZipStore)) return(invisible())
    keys <- store_list(store, "")
    node_keys <- keys[endsWith(keys, "/zarr.json")]   # root "zarr.json" excluded
    node_json <- list()
    for (k in node_keys) {
        node_json[[sub("/zarr.json$", "", k)]] <-
            rawToChar(store_get_bytes(store, k))
    }
    cache <- .zarr_v3_cache(store)
    if (!is.null(cache)) cache$cjson <- node_json
    .zarr_v3_write_root_json(store, node_json)
}

# Incrementally update the root index for the subtree at `base` (the node plus
# any sparse children) and its ancestor groups, without re-scanning or
# re-serializing the whole store - the O(N^2)-avoiding hot path for set_*. The
# index (raw node JSON strings) is held in the store's in-memory cache (rebuilt
# from disk on first use). Drops the prior subtree at `base` first so an
# overwrite (e.g. sparse -> dense, where stale nzind/nzval must disappear) stays
# consistent. No-op on zip stores.
zarr_v3_consolidate_upsert <- function(store, base) {
    if (S7::S7_inherits(store, MmapZipStore)) return(invisible())
    cache <- .zarr_v3_cache(store)
    node_json <- if (!is.null(cache)) cache$cjson else NULL
    if (is.null(node_json)) return(zarr_v3_write_consolidated(store))  # warm it
    prefix <- paste0(base, "/")
    node_json <- node_json[!(names(node_json) == base |
                                 startsWith(names(node_json), prefix))]
    for (k in store_list(store, base)) {
        if (!endsWith(k, "/zarr.json")) next
        node_json[[sub("/zarr.json$", "", k)]] <-
            rawToChar(store_get_bytes(store, k))
    }
    # Ancestor groups (e.g. "vectors", "vectors/<axis>") may be newly created by
    # this write; pull each into the index if not already present.
    parts <- strsplit(base, "/", fixed = TRUE)[[1L]]
    for (i in seq_len(length(parts) - 1L)) {
        anc <- paste(parts[seq_len(i)], collapse = "/")
        if (is.null(node_json[[anc]])) {
            raw <- store_get_bytes(store, paste0(anc, "/zarr.json"))
            if (!is.null(raw)) node_json[[anc]] <- rawToChar(raw)
        }
    }
    if (!is.null(cache)) cache$cjson <- node_json
    .zarr_v3_write_root_json(store, node_json)
}

# ---- sharded array zarr.json (packed write) ------------------------------

# Inner-codec list element for the sharding_indexed codec [pinned against zpk
# fixtures]. Key observations from fixtures:
#   - zstd: {level} only, no checksum key
#   - gzip: name "gzip", {level} only
#   - blosc: {blocksize, clevel, cname, shuffle, typesize} (Julia alphabetical,
#     but R/jsonlite will serialize in insertion order - structure is equivalent)
#   - crc32c: NO configuration key at all
.zarr_v3_inner_codec_json <- function(codec, level, typesize) {
    switch(codec,
        gzip = list(name = "gzip", configuration = list(level = level)),
        zstd = list(name = "zstd", configuration = list(level = level)),
        blosc_zstd_bitshuffle = list(name = "blosc", configuration = list(
            cname = "zstd", clevel = level, shuffle = "bitshuffle",
            typesize = typesize, blocksize = 0L)),
        blosc_lz4_bitshuffle = list(name = "blosc", configuration = list(
            cname = "lz4", clevel = level, shuffle = "bitshuffle",
            typesize = typesize, blocksize = 0L)),
        stop(sprintf("zarr_v3: unknown packed codec %s", sQuote(codec)),
             call. = FALSE))
}

# Zarr v3 array metadata for a packed (sharded) array. `shape` and `inner` are
# in on-disk order (for matrices: reversed [ncol, nrow]). One outer chunk covers
# the full shape. `codec` is one of: gzip, zstd, blosc_zstd_bitshuffle,
# blosc_lz4_bitshuffle. `level` is the compressor level.
zarr_v3_sharded_array_meta <- function(shape, dtype, inner, codec, level) {
    shape <- as.integer(shape)
    inner <- as.integer(inner)
    typesize <- if (dtype == "string") 1L else zarr_v3_size_for_dtype(dtype)
    # array->bytes step: vlen-utf8 for strings, bytes (little-endian) otherwise
    array_step <- if (dtype == "string") {
        list(name = "vlen-utf8",
             configuration = structure(list(), names = character(0L)))
    } else {
        list(name = "bytes", configuration = list(endian = "little"))
    }
    fill_value <- if (dtype == "string") "" else
        if (dtype == "bool") FALSE else
        if (zarr_v3_r_kind_for_dtype(dtype) == "double") 0.0 else 0L
    # crc32c carries no configuration key (pinned from fixtures)
    list(
        zarr_format = 3L,
        node_type = "array",
        shape = as.list(shape),
        data_type = dtype,
        chunk_grid = list(name = "regular",
                          configuration = list(chunk_shape = as.list(shape))),
        chunk_key_encoding = list(name = "default",
                                  configuration = list(separator = "/")),
        codecs = list(list(
            name = "sharding_indexed",
            configuration = list(
                chunk_shape = as.list(inner),
                codecs = list(
                    array_step,
                    .zarr_v3_inner_codec_json(codec, level, typesize)),
                index_codecs = list(
                    list(name = "bytes",
                         configuration = list(endian = "little")),
                    list(name = "crc32c")),
                index_location = "start"))),
        fill_value = fill_value,
        attributes = list(daf_packed_format = "indexed+zipped")
    )
}

# ---- chunk encode (write) ------------------------------------------------

# Encode an R vector to raw little-endian bytes for a v3 numeric dtype.
# Strings use zarr_v3_encode_strings (vlen-utf8).
zarr_v3_encode_chunk <- function(value, dtype) {
    if (dtype == "string") {
        stop("zarr_v3_encode_chunk: use zarr_v3_encode_strings for strings",
             call. = FALSE)
    }
    if (dtype == "float64") return(.zarr_v3_writeBin(as.double(value), 8L))
    if (dtype == "int32")   return(.zarr_v3_writeBin(as.integer(value), 4L))
    if (dtype == "int64") {
        if (!inherits(value, "integer64")) value <- bit64::as.integer64(value)
        return(.zarr_v3_writeBin(unclass(value), 8L, what = "double"))
    }
    if (dtype == "bool") return(as.raw(as.integer(as.logical(value))))
    stop(sprintf("zarr_v3_encode_chunk: unsupported write dtype %s",
                 sQuote(dtype)), call. = FALSE)
}

.zarr_v3_writeBin <- function(value, size, what = NULL) {
    con <- rawConnection(raw(0L), "wb"); on.exit(close(con))
    if (is.null(what)) {
        writeBin(value, con, size = size, endian = "little")
    } else {
        writeBin(as(value, what), con, size = size, endian = "little")
    }
    rawConnectionValue(con)
}

# ---- chunk decode (read) -------------------------------------------------

# Decode raw bytes to an R vector for a v3 numeric dtype. `compressor` carries
# over the v2 gzip path (DAF flat arrays are uncompressed; sharded/blosc is a
# separate plan). Strings use zarr_v3_decode_strings.
zarr_v3_decode_chunk <- function(bytes, dtype, n, compressor = NULL) {
    if (!is.null(compressor)) {
        # v3 codec specs are keyed by `name`; fall back to the legacy v2 `id`.
        codec_id <- compressor$name %||% compressor$id
        if (is.null(codec_id)) {
            stop("zarr_v3_decode_chunk: compressor has no name/id", call. = FALSE)
        }
        if (identical(codec_id, "gzip")) {
            bytes <- memDecompress(bytes, type = "gzip")
        } else {
            stop(sprintf("zarr_v3: codec %s not supported on the flat path",
                         sQuote(codec_id)), call. = FALSE)
        }
    }
    if (dtype == "string") {
        stop("zarr_v3_decode_chunk: use zarr_v3_decode_strings for strings",
             call. = FALSE)
    }
    con <- rawConnection(bytes, "rb"); on.exit(close(con))
    if (dtype == "float64") return(readBin(con, "double", n, 8L, endian = "little"))
    if (dtype == "float32") return(readBin(con, "double", n, 4L, endian = "little"))
    if (dtype == "int32")   return(readBin(con, "integer", n, 4L, endian = "little"))
    if (dtype %in% c("uint8", "int8")) {
        return(readBin(con, "integer", n, 1L, signed = (dtype == "int8")))
    }
    if (dtype %in% c("uint16", "int16")) {
        return(readBin(con, "integer", n, 2L, signed = (dtype == "int16"),
                       endian = "little"))
    }
    if (dtype == "uint32") {
        x <- readBin(con, "integer", n, 4L, endian = "little")
        neg <- !is.na(x) & x < 0L
        if (any(neg)) { x <- as.double(x); x[neg] <- x[neg] + 2^32 }
        return(x)
    }
    if (dtype %in% c("int64", "uint64")) {
        d <- readBin(con, "double", n, 8L, endian = "little")
        class(d) <- "integer64"
        return(d)
    }
    if (dtype == "bool") return(as.logical(readBin(con, "raw", n)))
    stop(sprintf("zarr_v3_decode_chunk: unsupported data_type %s",
                 sQuote(dtype)), call. = FALSE)
}

# ---- vlen-utf8 strings (numcodecs wire format; identical to v2) ----------
# [N: uint32 LE] then per string: [len_i: uint32 LE][utf8 bytes].

zarr_v3_encode_strings <- function(strings) {
    if (!is.character(strings)) {
        stop("zarr_v3_encode_strings: input must be a character vector",
             call. = FALSE)
    }
    con <- rawConnection(raw(0L), "wb"); on.exit(close(con))
    writeBin(length(strings), con, size = 4L, endian = "little")
    for (s in strings) {
        if (is.na(s)) stop("zarr_v3_encode_strings: NA not supported",
                           call. = FALSE)
        b <- charToRaw(enc2utf8(s))
        writeBin(length(b), con, size = 4L, endian = "little")
        if (length(b) > 0L) writeBin(b, con)
    }
    rawConnectionValue(con)
}

zarr_v3_decode_strings <- function(bytes, n = NA_integer_) {
    if (length(bytes) < 4L) {
        stop("zarr_v3_decode_strings: truncated (no count header)",
             call. = FALSE)
    }
    con <- rawConnection(bytes, "rb"); on.exit(close(con))
    n_in <- readBin(con, "integer", 1L, 4L, endian = "little")
    if (!is.na(n) && n_in != n) {
        stop(sprintf("zarr_v3_decode_strings: count %d != expected %d",
                     n_in, n), call. = FALSE)
    }
    out <- character(n_in)
    for (i in seq_len(n_in)) {
        len_i <- readBin(con, "integer", 1L, 4L, endian = "little")
        if (length(len_i) != 1L) {
            stop(sprintf("zarr_v3_decode_strings: truncated length at %d", i),
                 call. = FALSE)
        }
        if (len_i == 0L) { out[[i]] <- ""; next }
        payload <- readBin(con, "raw", len_i)
        if (length(payload) != len_i) {
            stop(sprintf("zarr_v3_decode_strings: truncated string at %d", i),
                 call. = FALSE)
        }
        out[[i]] <- rawToChar(payload); Encoding(out[[i]]) <- "UTF-8"
    }
    out
}
