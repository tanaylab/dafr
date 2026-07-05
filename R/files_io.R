# Low-level I/O helpers shared by FilesDaf read/write methods.
# - dtype table maps R-side types <-> on-disk Julia-style type strings.
# - path helpers translate (root, axis, name) triples to absolute paths.

.DTYPE_SIZES <- c(
    Bool    = 1L,
    Int8    = 1L,  UInt8  = 1L,
    Int16   = 2L,  UInt16 = 2L,
    Int32   = 4L,  UInt32 = 4L,
    Int64   = 8L,  UInt64 = 8L,
    Float32 = 4L, Float64 = 8L
)

.dtype_canonical <- function(x) {
    stopifnot(is.character(x), length(x) == 1L, !is.na(x))
    lower <- tolower(x)
    mapping <- c(
        bool = "Bool",
        int8 = "Int8", uint8 = "UInt8",
        int16 = "Int16", uint16 = "UInt16",
        int32 = "Int32", uint32 = "UInt32",
        int64 = "Int64", uint64 = "UInt64",
        float32 = "Float32", float64 = "Float64",
        string = "String",
        int = "Int64"
    )
    out <- mapping[lower]
    if (is.na(out)) {
        stop(sprintf("files_daf: unsupported type %s", sQuote(x)), call. = FALSE)
    }
    unname(out)
}

.dtype_size <- function(dtype) {
    dtype <- .dtype_canonical(dtype)
    if (dtype == "String") {
        stop("files_daf: String has no fixed byte size", call. = FALSE)
    }
    unname(.DTYPE_SIZES[[dtype]])
}

# Total on-disk byte count for `n` elements of `dtype`, always as a double.
# Used by the truncation checks in files_daf_read.R. Avoids int32 overflow
# on Float64 matrices with n*8 > 2^31 (e.g. a 15176 x 19867 atlas: n=3.0e8,
# bytes = 2.4e9 - past .Machine$integer.max).
.bytes_for_count <- function(n, dtype) {
    as.numeric(n) * as.numeric(.dtype_size(dtype))
}

.dtype_for_r_vector <- function(v) {
    if (is.logical(v)) {
        return("Bool")
    }
    if (inherits(v, "integer64")) {
        return("Int64")
    }
    if (is.integer(v)) {
        return("Int32")
    }
    if (is.double(v)) {
        return("Float64")
    }
    if (is.character(v)) {
        return("String")
    }
    stop(sprintf(
        "files_daf: cannot map R type %s to on-disk dtype",
        sQuote(typeof(v))
    ), call. = FALSE)
}

.path_scalar <- function(root, name) file.path(root, "scalars", paste0(name, ".json"))
.path_axis <- function(root, axis) file.path(root, "axes", paste0(axis, ".txt"))
.path_vector_dir <- function(root, axis) file.path(root, "vectors", axis)
.path_matrix_dir <- function(root, rows_axis, columns_axis) {
    file.path(root, "matrices", rows_axis, columns_axis)
}

# ---- JSON descriptors ----
# Each descriptor has a *string* builder (the exact bytes written, incl. trailing
# newline) reused by both the file writer (`.write_descriptor_*`) and ZipDaf
# (which charToRaw's the string into the store). Single source of truth.
.descriptor_dense_string <- function(dtype) {
    sprintf('{"format":"dense","eltype":"%s"}\n', dtype)
}
.write_descriptor_dense <- function(path, dtype) {
    cat(.descriptor_dense_string(dtype), file = path)
}

# Serialize a packed dense descriptor list (from .files_packed_descriptor).
# jsonlite renders the field order as inserted, which is byte-identical to the
# DataAxesFormats.jl FilesFormat packed descriptor (verified against the fpk
# fixtures).
.descriptor_packed_string <- function(desc) {
    paste0(jsonlite::toJSON(desc, auto_unbox = TRUE), "\n")
}
.write_descriptor_packed <- function(path, desc) {
    cat(.descriptor_packed_string(desc), file = path)
}

# Newline-delimited text payload (axes .txt, dense/sparse string vectors .txt/
# .nztxt), matching `writeLines(x, <wb UTF-8 con>)`: each element followed by a
# single LF. `.decode_lines` mirrors `readLines` (a single trailing LF does not
# produce an extra empty element).
.encode_lines <- function(x) {
    if (length(x) == 0L) return(raw(0L))
    charToRaw(paste0(x, "\n", collapse = ""))
}
.decode_lines <- function(bytes) {
    if (length(bytes) == 0L) return(character(0L))
    s <- rawToChar(bytes)
    Encoding(s) <- "UTF-8"
    s <- sub("\n$", "", s)
    if (!nzchar(s)) return(character(0L))
    strsplit(s, "\n", fixed = TRUE)[[1L]]
}

# Write a FilesFormat v1.1 sparse property descriptor: a per-component object
# for each index/value component, in the given order. `components` is a list of
# per-component descriptor objects, each either:
#   - flat:   list(key=, eltype=, n_elements=)  ->  rendered as a stand-alone
#             dense vector descriptor (matching DataAxesFormats.jl 0.3.0); the
#             binary payload file is unchanged from v1.0.
#   - packed: list(key=, desc=<.files_packed_descriptor list incl. n_elements>)
#             -> rendered as the nested packed shard descriptor (the payload is a
#             `<name>.<component>.zip` shard).
.descriptor_sparse_string <- function(components) {
    parts <- vapply(components, function(c) {
        if (!is.null(c$desc)) {
            # Packed component: render the packed descriptor list as a nested
            # object. jsonlite preserves the insertion field order.
            sprintf('"%s":%s', c$key,
                    jsonlite::toJSON(c$desc, auto_unbox = TRUE))
        } else {
            sprintf('"%s":{"format":"dense","eltype":"%s","n_elements":%d}',
                    c$key, c$eltype, as.integer(c$n_elements))
        }
    }, character(1L))
    sprintf('{"format":"sparse",%s}\n', paste(parts, collapse = ","))
}
.write_descriptor_sparse <- function(path, components) {
    cat(.descriptor_sparse_string(components), file = path)
}

# Build an in-memory packed dense-component descriptor (the R list that will be
# serialized to a `<name>.json` sidecar alongside a `<name>.zip` shard).
# Mirrors what DataAxesFormats.jl writes for a FilesFormat packed component.
# `n`      - element count; included as `n_elements` when non-NULL (required for
#            packed sub-descriptors inside a sparse property descriptor, omitted
#            for standalone dense vector/matrix packed descriptors).
# `inner`  - inner chunk shape (integer scalar or vector, e.g. 1024L or c(1024L, 1L)).
# `codec`  - FilesFormat compression name (e.g. "gzip", "zstd", "blosc_lz4_bitshuffle").
# `level`  - compression level integer.
.files_packed_descriptor <- function(eltype, n = NULL, inner, codec, level) {
    d <- list(
        format         = "dense",
        eltype         = eltype
    )
    if (!is.null(n)) d$n_elements <- as.integer(n)
    d$packed_format       <- "indexed+zipped"
    d$chunk_shape         <- as.list(as.integer(inner))
    d$compression         <- codec
    d$compression_level   <- as.integer(level)
    d$index_location      <- "start"
    d
}

# ---- packed component writers ----

# Write one 1-D component (a dense vector payload, or a sparse index/value
# component) either packed or flat, and return its descriptor list.
#   `base`     - path stem the packed `<base>.zip` shard is written at, e.g.
#                ".../vectors/cell/score" (dense vector -> score.zip) or
#                ".../vectors/cell/score.nzind" (sparse comp -> score.nzind.zip).
#   `ext`      - flat-payload extension appended to `base` to form the flat path.
#                For a dense vector this is ".data" (base lacks it, so flat is
#                score.data while packed strips to score.zip); for a sparse comp
#                `base` already carries the `.<comp>` stem so `ext` is "" (flat is
#                score.nzind, packed is score.nzind.zip).
#   `values`   - the element vector to write.
#   `dtype`    - Zarr v3 dtype name (lowercase, e.g. "float64") used by the shard
#                encoder.
#   `eltype`   - Julia eltype name (e.g. "Float64") stamped into the descriptor.
#   `packed`   - whether packed write is enabled for this store.
#   `include_n`- whether to stamp `n_elements` into the descriptor. Sparse
#                sub-components REQUIRE it (the reader sizes the shard from it);
#                a stand-alone dense vector OMITS it (Julia parity - the reader
#                sizes from the axis length).
# When packed AND over threshold the payload is a `<base>.zip` shard; otherwise a
# flat `<base><ext>` file. The returned descriptor is the per-component object the
# caller serializes (directly for a dense vector, or nested for a sparse comp).
# Pure: decide packed-vs-flat and return list(ext=, bytes=, desc=) - the on-disk
# extension, its payload bytes, and the component descriptor. No I/O. Shared by
# `.files_write_component` (writes to a path) and ZipDaf (writes to the store).
.files_component_bytes <- function(ext, values, dtype, eltype, packed,
                                   include_n = FALSE) {
    opts <- .packed_opts()
    n <- length(values)
    if (packed && .shard_should_pack(n, dtype, opts$target_kb)) {
        .packed_validate_codec(opts$compression)
        inner <- .shard_inner_chunk_shape(n, dtype, opts$target_kb)  # scalar
        blob <- .shard_assemble(values, dtype, n, inner,
                                opts$compression, opts$level)
        return(list(ext = ".zip", bytes = blob,
            desc = .files_packed_descriptor(
                eltype, n = if (include_n) n else NULL, inner = inner,
                codec = opts$compression, level = opts$level)))
    }
    d <- list(format = "dense", eltype = eltype)
    if (include_n) d$n_elements <- as.integer(n)
    list(ext = ext, bytes = .encode_dense(values, dtype), desc = d)
}

.files_write_component <- function(base, ext, values, dtype, eltype, packed,
                                   include_n = FALSE) {
    r <- .files_component_bytes(ext, values, dtype, eltype, packed, include_n)
    writeBin(r$bytes, paste0(base, r$ext))
    r$desc
}

# Write a dense matrix payload either packed or flat, and return its descriptor
# list. On-disk the element stream is column-major over the natural [nrow, ncol]
# matrix. The packed shard is assembled in the SAME convention as ZarrDaf (and as
# the Julia FilesFormat writer): the reversed on-disk shape [ncol, nrow] with the
# inner chunk tiling the fast (nrow) dimension as [1, nrow_chunk], which yields a
# column-grouped inner-chunk stream. The descriptor's `chunk_shape`, however, is
# the NATURAL [nrow_chunk, 1] (matching the fpk fixtures and the column-major
# decoder in .files_packed_decode_matrix). Threshold is on nrow (the fast dim).
# Pure: return list(bytes=, desc=) for a packed dense matrix (no I/O). Shared.
.files_dense_matrix_packed_bytes <- function(mat, dtype, eltype) {
    opts <- .packed_opts()
    nr <- nrow(mat)
    nc <- ncol(mat)
    .packed_validate_codec(opts$compression)
    nrowchunk <- .shard_slab_rows(dtype, opts$target_kb, nr)
    blob <- .shard_assemble(as.vector(mat), dtype,
                            shape = c(as.integer(nc), as.integer(nr)),
                            inner = c(1L, nrowchunk),
                            opts$compression, opts$level)
    list(bytes = blob,
         desc = .files_packed_descriptor(eltype, n = NULL,
                                         inner = c(nrowchunk, 1L),
                                         codec = opts$compression, level = opts$level))
}

.files_write_dense_matrix_packed <- function(base, mat, dtype, eltype) {
    r <- .files_dense_matrix_packed_bytes(mat, dtype, eltype)
    writeBin(r$bytes, paste0(base, ".zip"))
    r$desc
}

# TRUE if a dense matrix's fast (nrow) dimension is over the pack threshold.
.files_matrix_should_pack <- function(nr, dtype) {
    .shard_should_pack(nr, dtype, .packed_opts()$target_kb)
}

# Fast-path regex for the two fixed descriptor schemas dafr emits:
#   {"format":"dense","eltype":"X"}
#   {"format":"sparse","eltype":"X","indtype":"Y"}
# Falls back to jsonlite on any mismatch.
.DESCRIPTOR_DENSE_RE <- paste0(
    '^\\{"format":"(dense)","eltype":"([A-Za-z0-9]+)"\\}\\s*$'
)
.DESCRIPTOR_SPARSE_RE <- paste0(
    '^\\{"format":"(sparse)","eltype":"([A-Za-z0-9]+)",',
    '"indtype":"([A-Za-z0-9]+)"\\}\\s*$'
)

.read_descriptor <- function(path) {
    .decode_descriptor_bytes(readBin(path, "raw", n = file.size(path)))
}

.decode_descriptor_bytes <- function(bytes) {
    raw <- rawToChar(bytes)
    # Try dense fast-path
    m <- regmatches(raw, regexec(.DESCRIPTOR_DENSE_RE, raw, perl = TRUE))[[1L]]
    if (length(m) == 3L) {
        return(list(
            format  = m[[2L]],
            eltype  = .dtype_canonical(m[[3L]]),
            indtype = NULL
        ))
    }
    # Try sparse fast-path
    m <- regmatches(raw, regexec(.DESCRIPTOR_SPARSE_RE, raw, perl = TRUE))[[1L]]
    if (length(m) == 4L) {
        return(list(
            format  = m[[2L]],
            eltype  = .dtype_canonical(m[[3L]]),
            indtype = .dtype_canonical(m[[4L]])
        ))
    }
    # Fallback: full JSON parse
    j <- jsonlite::fromJSON(raw, simplifyVector = TRUE)
    fmt <- j$format
    elt <- j$eltype
    if (is.null(fmt) || !(fmt %in% c("dense", "sparse"))) {
        stop("files_daf: malformed descriptor (no format)", call. = FALSE)
    }
    if (fmt == "sparse" && is.null(elt)) {
        # FilesFormat v1.1 (DataAxesFormats.jl 0.3.0): no top-level eltype;
        # per-component descriptors (nzind/nzval or colptr/rowval/nzval) carry
        # the dtypes. Return the full descriptor so the sparse readers derive
        # eltype/indtype via .files_parse_sparse_descriptor.
        return(j)
    }
    if (!is.null(j$packed_format)) {
        # Packed (chunked + compressed) dense property (DataAxesFormats.jl
        # 0.3.0): return the full descriptor (packed_format / chunk_shape /
        # compression / eltype) so the packed readers in R/files_packed.R can
        # route and decode the `.zip` shard.
        return(j)
    }
    if (is.null(elt)) {
        stop("files_daf: malformed descriptor (no eltype)", call. = FALSE)
    }
    list(
        format = fmt, eltype = .dtype_canonical(elt),
        indtype = if (is.null(j$indtype)) NULL else .dtype_canonical(j$indtype)
    )
}

# ---- scalar JSON ----
# The scalar JSON *string* (including its trailing newline) is built by
# `.scalar_json_string`; `.write_scalar_json` writes it to a file and
# `.encode_scalar_json` returns its raw bytes (for ZipDaf). Single source.
.scalar_json_string <- function(value) {
    dtype <- .dtype_for_r_vector(value)
    if (dtype == "String") {
        obj <- list(
            type = jsonlite::unbox("String"),
            value = jsonlite::unbox(value)
        )
    } else if (dtype == "Bool") {
        obj <- list(
            type = jsonlite::unbox("Bool"),
            value = jsonlite::unbox(as.integer(value))
        )
    } else if (dtype == "Int64") {
        return(sprintf(
            '{"type":"Int64","value":%s}\n',
            format(value, scientific = FALSE)
        ))
    } else {
        obj <- list(
            type = jsonlite::unbox(dtype),
            value = jsonlite::unbox(value)
        )
    }
    # digits = 17 is the minimum needed to round-trip Float64 without
    # precision loss. jsonlite's default of 4 was truncating high-
    # precision scalars (e.g. pi to 3.1416). String/Bool branches are
    # unaffected; the explicit Int64 branch above returns its own JSON.
    paste0(jsonlite::toJSON(obj, auto_unbox = FALSE, digits = 17), "\n")
}

.write_scalar_json <- function(path, value) {
    cat(.scalar_json_string(value), file = path)
}

.encode_scalar_json <- function(value) charToRaw(.scalar_json_string(value))

# Fast-path regex for fixed scalar schemas dafr emits:
#   {"type":"X","value":N}       numeric/bool  (N = [-+digits.eE]+)
#   {"type":"X","value":"S"}     string         (S = no backslash, no quote, no control)
# Falls back to jsonlite on any mismatch (escapes, whitespace, extra fields, …).
.SCALAR_NUM_RE  <- '^\\{"type":"([A-Za-z0-9]+)","value":(-?[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?)\\}\\s*$'
.SCALAR_STR_RE  <- '^\\{"type":"([A-Za-z0-9]+)","value":"([^"\\\\[:cntrl:]]*)"\\}\\s*$'

.decode_scalar_json <- function(bytes) {
    raw <- rawToChar(bytes)
    # Try numeric/bool fast-path
    m <- regmatches(raw, regexec(.SCALAR_NUM_RE, raw, perl = TRUE))[[1L]]
    if (length(m) == 3L) {
        t <- .dtype_canonical(m[[2L]])
        v <- m[[3L]]
        return(switch(t,
            Bool    = as.logical(as.integer(v)),
            Int8    = ,
            Int16   = ,
            Int32   = as.integer(v),
            Int64   = bit64::as.integer64(v),
            UInt8   = ,
            UInt16  = ,
            UInt32  = as.integer(v),
            UInt64  = bit64::as.integer64(v),
            Float32 = as.double(v),
            Float64 = as.double(v),
            # Unknown numeric type — fall through to jsonlite
            NULL
        ))
    }
    # Try string fast-path
    m <- regmatches(raw, regexec(.SCALAR_STR_RE, raw, perl = TRUE))[[1L]]
    if (length(m) == 3L) {
        t <- .dtype_canonical(m[[2L]])
        if (t == "String") {
            # rawToChar leaves the string with Encoding "bytes"/"unknown". dafr
            # files are UTF-8 by spec; tag explicitly so downstream `Encoding()`
            # / `serialize()` agree with the memory backend.
            s <- m[[3L]]
            Encoding(s) <- "UTF-8"
            return(s)
        }
        # Non-string type with quoted value — fall through
    }
    # Fallback: full JSON parse
    j <- jsonlite::fromJSON(raw, simplifyVector = TRUE)
    t <- .dtype_canonical(j$type)
    v <- j$value
    switch(t,
        Bool = as.logical(v),
        Int8 = ,
        Int16 = ,
        Int32 = as.integer(v),
        Int64 = bit64::as.integer64(v),
        UInt8 = ,
        UInt16 = ,
        UInt32 = as.integer(v),
        UInt64 = bit64::as.integer64(v),
        Float32 = as.double(v),
        Float64 = as.double(v),
        String = as.character(v),
        stop(sprintf("files_daf: unsupported scalar type %s", t))
    )
}

.read_scalar_json <- function(path) {
    .decode_scalar_json(readBin(path, "raw", n = file.size(path)))
}

# ---- binary I/O ----
# Dense encode/decode is split into connection-based cores (`.write_dense_con` /
# `.read_dense_con`) so the SAME dtype switch serves both file I/O (FilesDaf) and
# raw-bytes I/O (ZipDaf, via `.encode_dense` / `.decode_dense` over a
# rawConnection). A rawConnection produces byte-identical output to a file
# connection, so this is a pure refactor.
.write_dense_con <- function(con, value, dtype) {
    dtype <- .dtype_canonical(dtype)
    switch(dtype,
        Bool    = writeBin(as.raw(as.integer(value)), con),
        Int8    = writeBin(as.integer(value), con, size = 1L, endian = "little"),
        Int16   = writeBin(as.integer(value), con, size = 2L, endian = "little"),
        Int32   = writeBin(as.integer(value), con, size = 4L, endian = "little"),
        Int64   = writeBin(unclass(bit64::as.integer64(value)), con, size = 8L, endian = "little"),
        UInt8   = writeBin(as.integer(value), con, size = 1L, endian = "little"),
        UInt16  = writeBin(as.integer(value), con, size = 2L, endian = "little"),
        UInt32  = writeBin(as.integer(value), con, size = 4L, endian = "little"),
        UInt64  = writeBin(unclass(bit64::as.integer64(value)), con, size = 8L, endian = "little"),
        Float32 = writeBin(as.double(value), con, size = 4L, endian = "little"),
        Float64 = writeBin(as.double(value), con, size = 8L, endian = "little"),
        stop(sprintf("files_daf: unsupported dtype %s for dense write", dtype))
    )
    invisible()
}

.read_dense_con <- function(con, n, dtype) {
    dtype <- .dtype_canonical(dtype)
    switch(dtype,
        Bool = as.logical(readBin(con,
            what = "integer", n = n, size = 1L,
            signed = FALSE, endian = "little"
        )),
        Int8 = readBin(con,
            what = "integer", n = n, size = 1L, signed = TRUE,
            endian = "little"
        ),
        Int16 = readBin(con,
            what = "integer", n = n, size = 2L, signed = TRUE,
            endian = "little"
        ),
        Int32 = readBin(con,
            what = "integer", n = n, size = 4L, signed = TRUE,
            endian = "little"
        ),
        Int64 = {
            # R's `readBin(what="integer", size=8L)` truncates each value
            # to its low 32 bits because base R has no native 8-byte int.
            # Read 8-byte doubles and bit-alias into integer64 (same bit
            # storage as bit64::as.integer64).
            raw_dbl <- readBin(con, what = "double", n = n, size = 8L, endian = "little")
            oldClass(raw_dbl) <- "integer64"
            raw_dbl
        },
        UInt8 = readBin(con,
            what = "integer", n = n, size = 1L, signed = FALSE,
            endian = "little"
        ),
        UInt16 = readBin(con,
            what = "integer", n = n, size = 2L, signed = FALSE,
            endian = "little"
        ),
        UInt32 = readBin(con,
            what = "integer", n = n, size = 4L, signed = TRUE,
            endian = "little"
        ),
        UInt64 = {
            # Same low-32-bits truncation as Int64; bit-alias via double.
            raw_dbl <- readBin(con, what = "double", n = n, size = 8L, endian = "little")
            oldClass(raw_dbl) <- "integer64"
            raw_dbl
        },
        Float32 = readBin(con, what = "double", n = n, size = 4L, endian = "little"),
        Float64 = readBin(con, what = "double", n = n, size = 8L, endian = "little"),
        stop(sprintf("files_daf: unsupported dtype %s for dense read", dtype))
    )
}

.write_bin_dense <- function(path, value, dtype) {
    con <- file(path, open = "wb")
    on.exit(close(con), add = TRUE)
    .write_dense_con(con, value, dtype)
    invisible()
}

.read_bin_dense <- function(path, n, dtype) {
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    .read_dense_con(con, n, dtype)
}

# Pure: encode a dense R vector to little-endian raw bytes (no file I/O).
.encode_dense <- function(value, dtype) {
    con <- rawConnection(raw(0L), "wb")
    on.exit(close(con), add = TRUE)
    .write_dense_con(con, value, dtype)
    rawConnectionValue(con)
}

# Pure: decode little-endian raw bytes to a dense R vector (no file I/O).
.decode_dense <- function(bytes, n, dtype) {
    con <- rawConnection(bytes, "rb")
    on.exit(close(con), add = TRUE)
    .read_dense_con(con, n, dtype)
}

.indtype_for_size <- function(size) {
    # Smallest unsigned int holding `size`, matching Julia's
    # TanayLabUtilities.indtype_for_size (floor UInt16). dafr caps UInt32 at R's
    # native integer.max (2^31-1) rather than Julia's typemax(UInt32) (2^32-1),
    # because an index value >= 2^31 cannot be held in R's signed 32-bit int.
    if (size <= 65535) {
        "UInt16"
    } else if (size <= .Machine$integer.max) {
        "UInt32"
    } else {
        "UInt64"
    }
}

# ---- sparsify heuristics (Julia spec §8 / §8.4) ----

.should_sparsify_numeric <- function(vec, eltype, indtype) {
    n <- length(vec)
    if (n == 0L) {
        return(FALSE)
    }
    nnz <- if (is.logical(vec)) {
        sum(vec, na.rm = TRUE)
    } else {
        # NaN is a real Float64 value (not zero); preserve it through the
        # sparsify decision and the sparse write below.
        sum(is.nan(vec) | (vec != 0), na.rm = TRUE)
    }
    sparse_bytes <- nnz * (.dtype_size(eltype) + .dtype_size(indtype))
    dense_bytes <- n * .dtype_size(eltype)
    sparse_bytes <= 0.75 * dense_bytes
}

.should_sparsify_string <- function(vec, indtype) {
    n <- length(vec)
    if (n == 0L) {
        return(FALSE)
    }
    nonempty <- nzchar(vec)
    n_nonempty <- sum(nonempty)
    nonempty_bytes <- sum(nchar(vec[nonempty], type = "bytes"))
    sparse_size <- nonempty_bytes + n_nonempty * (1L + .dtype_size(indtype))
    dense_size <- nonempty_bytes + n
    sparse_size <= 0.75 * dense_size
}
