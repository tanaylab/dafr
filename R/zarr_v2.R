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
