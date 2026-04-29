# Zarr v2 metadata + chunk I/O layer. Operates against any ZarrStore
# from R/zarr_store.R; this file does no I/O of its own beyond the
# Store interface.
#
# Phase 2 (this commit): .zarray + .zattrs JSON + dtype mapping.
# Phase 3: numeric chunk encode/decode.
# Phase 4: vlen-utf8 string encoding.

# ---- dtype mapping (R type → Zarr v2 dtype string) -----------------------

# Returns a Zarr v2 dtype string for the given R value. Always
# little-endian.
zarr_v2_dtype_for_r <- function(value) {
    if (is.character(value)) return("|O")        # object dtype + vlen-utf8 filter
    if (is.logical(value)) return("|b1")
    if (inherits(value, "integer64")) return("<i8")
    if (is.integer(value)) return("<i4")
    if (is.numeric(value)) return("<f8")
    stop(sprintf("zarr_v2: cannot map R type %s to a Zarr dtype",
                 class(value)[1L]),
         call. = FALSE)
}

# Returns the R "kind" name for a Zarr v2 dtype string. Used by readers
# to know which readBin call to dispatch.
zarr_v2_r_kind_for_dtype <- function(dtype) {
    switch(dtype,
        "<f8" = "double",
        "<i4" = "integer",
        "<i8" = "integer64",
        "|b1" = "logical",
        "|O"  = "character",
        stop(sprintf("zarr_v2: unsupported dtype %s", sQuote(dtype)),
             call. = FALSE)
    )
}

# Returns the byte size of a single element for a fixed-width dtype,
# or NA_integer_ for variable-length dtypes (strings).
zarr_v2_size_for_dtype <- function(dtype) {
    switch(dtype,
        "<f8" = 8L,
        "<i4" = 4L,
        "<i8" = 8L,
        "|b1" = 1L,
        "|O"  = NA_integer_,
        stop(sprintf("zarr_v2: unsupported dtype %s", sQuote(dtype)),
             call. = FALSE)
    )
}

# ---- .zarray JSON --------------------------------------------------------

# Build a Zarr v2 .zarray descriptor. One chunk per array (chunks ==
# shape) by default — matches upstream's default and keeps the
# Phase-3 encoders simple.
zarr_v2_zarray <- function(shape, dtype,
                           chunks = NULL,
                           fill_value = NULL,
                           compressor = NULL,
                           filters = NULL,
                           order = "C",
                           dimension_separator = "/") {
    if (is.null(chunks)) {
        chunks <- if (length(shape) > 0L) shape else 1L
    }
    list(
        zarr_format = 2L,
        shape = as.list(as.integer(shape)),
        dtype = dtype,
        chunks = as.list(as.integer(chunks)),
        compressor = compressor,
        fill_value = fill_value,
        order = order,
        filters = filters,
        dimension_separator = dimension_separator
    )
}

# Write .zarray + (optional) .zattrs for a single array under `path`.
zarr_v2_write_zarray <- function(store, path, zarray) {
    json <- jsonlite::toJSON(zarray, auto_unbox = TRUE, null = "null",
                             pretty = FALSE)
    store_set_bytes(store, paste0(path, "/.zarray"),
                    charToRaw(as.character(json)))
    invisible()
}

zarr_v2_read_zarray <- function(store, path) {
    raw <- store_get_bytes(store, paste0(path, "/.zarray"))
    if (is.null(raw)) return(NULL)
    jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
}

zarr_v2_write_zattrs <- function(store, path, attrs) {
    if (length(attrs) == 0L) return(invisible())
    json <- jsonlite::toJSON(attrs, auto_unbox = TRUE, pretty = FALSE)
    store_set_bytes(store, paste0(path, "/.zattrs"),
                    charToRaw(as.character(json)))
    invisible()
}

zarr_v2_read_zattrs <- function(store, path) {
    raw <- store_get_bytes(store, paste0(path, "/.zattrs"))
    if (is.null(raw)) return(list())
    jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
}

# ---- .zmetadata consolidation -------------------------------------------
#
# A Zarr v2 store can carry a `.zmetadata` JSON at its root that
# mirrors every per-array `.zarray` + `.zattrs` (and `.zgroup`) in the
# tree. zarr-python uses this to load metadata in O(1) HTTP roundtrips.
# Upstream DataAxesFormats.jl writes this file too (see
# `refresh_consolidated_metadata!` in zarr_format.jl). dafr maintains
# this file always; on every set/delete the consolidated metadata is
# rewritten so live readers (HTTP-backed `ConsolidatedStore` clients in
# particular) observe a consistent view.
#
# JSON shape (matches upstream + zarr-python's `consolidate_metadata`):
#
#     {
#       "zarr_consolidated_format": 1,
#       "metadata": {
#         ".zgroup":             {"zarr_format": 2},     # root group (synthesised)
#         "scalars/.zgroup":     {"zarr_format": 2},     # intermediate (synthesised)
#         "scalars/n/.zarray":   {<contents>},
#         "axes/.zgroup":        {"zarr_format": 2},     # intermediate (synthesised)
#         "axes/cell/.zarray":   {<contents>},
#         ...
#       }
#     }
#
# zarr-python v3 `_flat_to_nested` requires intermediate `.zgroup` entries
# for every ancestor of an array.  dafr synthesises these here — they are
# not written as separate files; the consolidated copy is sufficient for
# `zarr.open()` HTTP/consolidated readers.  `.zattrs` entries are NOT
# synthesised: zarr-python works without them and omitting them preserves
# the "sparse vector has no .zattrs" invariant (Phase 8).
#
# zarr-python v3's `_flat_to_nested` requires that every intermediate
# directory prefix of an array also appear as a `.zgroup` entry;
# otherwise `zarr.open()` raises KeyError when rebuilding the tree.
# dafr synthesises the missing intermediate `.zgroup` entries here
# (they are not written as separate files; the consolidated copy is
# the canonical source for HTTP / consolidated readers).

# Walk every key matching *.zarray, *.zattrs, *.zgroup (including the
# bare root keys) and inline the parsed JSON into a single dict.
# Also synthesise `.zgroup` entries for every intermediate directory
# prefix so zarr-python v3 can rebuild the nested group tree.
zarr_v2_consolidated_metadata <- function(store) {
    keys <- store_list(store, "")
    # Match */.zarray, */.zattrs, */.zgroup (slash-prefixed) AND the
    # bare root variants (.zgroup, .zattrs) which have no leading slash.
    matching <- keys[endsWith(keys, "/.zarray") |
                     endsWith(keys, "/.zattrs") |
                     endsWith(keys, "/.zgroup") |
                     keys == ".zgroup" |
                     keys == ".zattrs"]
    metadata <- list()
    for (k in matching) {
        bytes <- store_get_bytes(store, k)
        if (is.null(bytes)) next
        json <- jsonlite::fromJSON(rawToChar(bytes), simplifyVector = FALSE)
        metadata[[k]] <- json
    }

    # Synthesise intermediate-group `.zgroup` entries.  For every
    # .zarray or .zgroup key of the form "a/b/c/.zarray" (depth >= 2)
    # we need ".zgroup" entries for every ancestor directory: "", "a",
    # "a/b", etc.  The root "" is rendered as ".zgroup" (bare).
    # Depth-1 arrays (e.g. "foo/.zarray") only need the root, which is
    # already ensured by `_init_store` writing the file.
    array_keys  <- keys[endsWith(keys, "/.zarray") | endsWith(keys, "/.zgroup")]
    # Extract directory paths from each array key.
    dir_paths <- unique(unlist(lapply(array_keys, function(k) {
        # Strip the trailing /.<meta> suffix to get the array/group path.
        path <- sub("/\\.[^/]+$", "", k)    # e.g. "scalars/n"
        parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
        n <- length(parts)
        if (n <= 1L) return(character(0L))  # depth 1: only root needed
        # Return all intermediate prefixes (depth 1 to n-1).
        vapply(seq_len(n - 1L), function(i) {
            paste(parts[seq_len(i)], collapse = "/")
        }, character(1L))
    })))
    # Ensure root is included whenever there are any arrays.
    if (length(array_keys) > 0L) dir_paths <- unique(c("", dir_paths))
    .zgroup_content <- list(zarr_format = 2L)
    for (dp in dir_paths) {
        zg_key <- if (nzchar(dp)) paste0(dp, "/.zgroup") else ".zgroup"
        if (!zg_key %in% names(metadata)) {
            metadata[[zg_key]] <- .zgroup_content
        }
    }

    list(zarr_consolidated_format = 1L, metadata = metadata)
}

# Rewrite the root-level `.zmetadata` file. Called at the END of every
# write/delete helper in zarr_format.R so the consolidated copy is
# always in sync with the per-file copies. No-op-friendly: if the
# store is empty the metadata dict is just `{}`.
zarr_v2_write_zmetadata <- function(store) {
    consolidated <- zarr_v2_consolidated_metadata(store)
    # `metadata` must serialize as `{}` (object), not `[]` (empty array),
    # when there are no entries. jsonlite handles named lists correctly;
    # an empty unnamed list serializes as `[]`. Force object shape.
    if (length(consolidated$metadata) == 0L) {
        # Empty named list serializes as {} only via auto_unbox plus an
        # explicit nudge; jsonlite's safest path is to drop into
        # named_list semantics by setting names() to character(0).
        consolidated$metadata <- structure(list(), names = character(0L))
    }
    json <- jsonlite::toJSON(consolidated, auto_unbox = TRUE,
                             null = "null", pretty = FALSE)
    store_set_bytes(store, ".zmetadata", charToRaw(as.character(json)))
    invisible()
}

zarr_v2_read_zmetadata <- function(store) {
    bytes <- store_get_bytes(store, ".zmetadata")
    if (is.null(bytes)) return(NULL)
    jsonlite::fromJSON(rawToChar(bytes), simplifyVector = FALSE)
}

# ---- chunk encode (write) ------------------------------------------------

# Encode an R vector to raw little-endian bytes for the given dtype.
# Strings (`|O`) error here — see Phase 4's string encoder.
zarr_v2_encode_chunk <- function(value, dtype) {
    if (dtype == "|O") {
        stop("zarr_v2_encode_chunk: use zarr_v2_encode_strings for vlen-utf8",
             call. = FALSE)
    }
    if (dtype == "<f8") {
        return(.zarr_v2_writeBin(as.double(value), size = 8L))
    }
    if (dtype == "<i4") {
        return(.zarr_v2_writeBin(as.integer(value), size = 4L))
    }
    if (dtype == "<i8") {
        # bit64::integer64 → little-endian raw via .Internal-free path.
        if (!inherits(value, "integer64")) {
            value <- bit64::as.integer64(value)
        }
        # bit64 stores integer64 as a double-typed R vector with class "integer64".
        # The on-disk bytes are the underlying little-endian int64 representation,
        # which equals the underlying double bits for that vector. writeBin with
        # size = 8L on the underlying double gives us the right bytes.
        return(.zarr_v2_writeBin(unclass(value), size = 8L,
                                 what = "double"))
    }
    if (dtype == "|b1") {
        # 1 byte per element, 0 or 1.
        return(as.raw(as.integer(as.logical(value))))
    }
    stop(sprintf("zarr_v2_encode_chunk: unsupported dtype %s", sQuote(dtype)),
         call. = FALSE)
}

.zarr_v2_writeBin <- function(value, size, what = NULL) {
    con <- rawConnection(raw(0L), "wb")
    on.exit(close(con))
    if (is.null(what)) {
        writeBin(value, con, size = size, endian = "little")
    } else {
        # Force the storage type for the writeBin call (used for integer64
        # → double-bit-pattern reinterpretation).
        writeBin(as(value, what), con, size = size, endian = "little")
    }
    rawConnectionValue(con)
}

# ---- chunk decode (read) -------------------------------------------------

# Decode raw bytes back to an R vector. Handles uncompressed + gzip.
# Errors clearly for unsupported codecs.
zarr_v2_decode_chunk <- function(bytes, dtype, n, compressor = NULL) {
    if (!is.null(compressor)) {
        codec_id <- compressor$id %||% compressor[["id"]] %||%
                    if (is.character(compressor)) compressor else NULL
        if (is.null(codec_id)) {
            stop("zarr_v2_decode_chunk: compressor field has no id", call. = FALSE)
        }
        if (identical(codec_id, "gzip")) {
            bytes <- memDecompress(bytes, type = "gzip")
        } else {
            stop(sprintf(
                "zarr_v2: codec %s not supported (only uncompressed + gzip); re-save with compressor=None",
                sQuote(codec_id)
            ), call. = FALSE)
        }
    }
    if (dtype == "|O") {
        stop("zarr_v2_decode_chunk: use zarr_v2_decode_strings for vlen-utf8",
             call. = FALSE)
    }
    con <- rawConnection(bytes, "rb")
    on.exit(close(con))
    if (dtype == "<f8") {
        return(readBin(con, "double", n = n, size = 8L, endian = "little"))
    }
    if (dtype == "<i4") {
        return(readBin(con, "integer", n = n, size = 4L, endian = "little"))
    }
    if (dtype == "<i8") {
        # Read as 8-byte doubles (R's only fixed-8-byte type without altrep tricks)
        # and reinterpret as integer64 by setting the class.
        d <- readBin(con, "double", n = n, size = 8L, endian = "little")
        class(d) <- "integer64"
        return(d)
    }
    if (dtype == "|b1") {
        return(as.logical(readBin(con, "raw", n = n)))
    }
    stop(sprintf("zarr_v2_decode_chunk: unsupported dtype %s", sQuote(dtype)),
         call. = FALSE)
}

# ---- vlen-utf8 strings ---------------------------------------------------
#
# Wire format (numcodecs.VLenUTF8):
#   [N: uint32 LE]                              -- count of strings
#   For each string i in 0..N-1:
#     [len_i: uint32 LE]                        -- byte length of UTF-8 encoded string
#     [utf8_bytes_i: bytes]                     -- UTF-8 payload (len_i bytes)
#
# Ground truth: numcodecs.VLenUTF8 in Python's zarr stack.
# Verified against fixture: see test-zarr-v2-strings.R (the 19-byte
# fixture for c("A","B","C") was generated with
# `python3 -c 'import numcodecs, numpy; ...'` — see Phase 4 notes).

zarr_v2_encode_strings <- function(strings) {
    if (!is.character(strings)) {
        stop("zarr_v2_encode_strings: input must be a character vector",
             call. = FALSE)
    }
    n <- length(strings)
    # Convert each string to its UTF-8 byte representation.
    utf8_bytes_list <- lapply(strings, function(s) {
        if (is.na(s)) {
            stop("zarr_v2_encode_strings: NA strings not supported",
                 call. = FALSE)
        }
        charToRaw(enc2utf8(s))
    })
    # Concatenate header + per-string records.
    con <- rawConnection(raw(0L), "wb")
    on.exit(close(con))
    writeBin(as.integer(n), con, size = 4L, endian = "little")
    for (b in utf8_bytes_list) {
        writeBin(as.integer(length(b)), con, size = 4L, endian = "little")
        if (length(b) > 0L) {
            writeBin(b, con)
        }
    }
    rawConnectionValue(con)
}

zarr_v2_decode_strings <- function(bytes, n = NA_integer_) {
    if (length(bytes) < 4L) {
        stop("zarr_v2_decode_strings: bytes truncated (no count header)",
             call. = FALSE)
    }
    con <- rawConnection(bytes, "rb")
    on.exit(close(con))
    n_in_chunk <- readBin(con, "integer", n = 1L, size = 4L, endian = "little")
    if (!is.na(n) && n_in_chunk != n) {
        stop(sprintf(
            "zarr_v2_decode_strings: chunk count %d differs from expected %d",
            n_in_chunk, n
        ), call. = FALSE)
    }
    out <- character(n_in_chunk)
    for (i in seq_len(n_in_chunk)) {
        len_i <- readBin(con, "integer", n = 1L, size = 4L, endian = "little")
        if (length(len_i) != 1L) {
            stop(sprintf(
                "zarr_v2_decode_strings: truncated length header at index %d",
                i
            ), call. = FALSE)
        }
        if (len_i < 0L) {
            stop(sprintf("zarr_v2_decode_strings: negative length %d at index %d",
                         len_i, i), call. = FALSE)
        }
        if (len_i == 0L) {
            out[[i]] <- ""
        } else {
            payload <- readBin(con, "raw", n = len_i)
            if (length(payload) != len_i) {
                stop(sprintf(
                    "zarr_v2_decode_strings: truncated string at index %d (%d/%d bytes)",
                    i, length(payload), len_i
                ), call. = FALSE)
            }
            out[[i]] <- rawToChar(payload)
            Encoding(out[[i]]) <- "UTF-8"
        }
    }
    out
}
