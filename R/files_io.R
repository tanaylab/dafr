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
.write_descriptor_dense <- function(path, dtype) {
    cat(sprintf('{"format":"dense","eltype":"%s"}\n', dtype), file = path)
}

.write_descriptor_sparse <- function(path, dtype, indtype) {
    cat(sprintf(
        '{"format":"sparse","eltype":"%s","indtype":"%s"}\n',
        dtype, indtype
    ), file = path)
}

.read_descriptor <- function(path) {
    j <- jsonlite::fromJSON(path, simplifyVector = TRUE)
    fmt <- j$format
    elt <- j$eltype
    if (is.null(fmt) || !(fmt %in% c("dense", "sparse"))) {
        stop(sprintf(
            "files_daf: %s has malformed descriptor (no format)",
            sQuote(path)
        ), call. = FALSE)
    }
    if (is.null(elt)) {
        stop(sprintf(
            "files_daf: %s has malformed descriptor (no eltype)",
            sQuote(path)
        ), call. = FALSE)
    }
    list(
        format = fmt, eltype = .dtype_canonical(elt),
        indtype = if (is.null(j$indtype)) NULL else .dtype_canonical(j$indtype)
    )
}

# ---- scalar JSON ----
.write_scalar_json <- function(path, value) {
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
        cat(
            sprintf(
                '{"type":"Int64","value":%s}\n',
                format(value, scientific = FALSE)
            ),
            file = path
        )
        return(invisible())
    } else {
        obj <- list(
            type = jsonlite::unbox(dtype),
            value = jsonlite::unbox(value)
        )
    }
    cat(jsonlite::toJSON(obj, auto_unbox = FALSE), "\n", file = path, sep = "")
}

.read_scalar_json <- function(path) {
    j <- jsonlite::fromJSON(path, simplifyVector = TRUE)
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

# ---- binary I/O ----
.write_bin_dense <- function(path, value, dtype) {
    dtype <- .dtype_canonical(dtype)
    con <- file(path, open = "wb")
    on.exit(close(con), add = TRUE)
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

.read_bin_dense <- function(path, n, dtype) {
    dtype <- .dtype_canonical(dtype)
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
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
            raw64 <- readBin(con, what = "integer", n = n, size = 8L, endian = "little")
            bit64::as.integer64(raw64)
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
            raw64 <- readBin(con, what = "integer", n = n, size = 8L, endian = "little")
            bit64::as.integer64(raw64)
        },
        Float32 = readBin(con, what = "double", n = n, size = 4L, endian = "little"),
        Float64 = readBin(con, what = "double", n = n, size = 8L, endian = "little"),
        stop(sprintf("files_daf: unsupported dtype %s for dense read", dtype))
    )
}

.indtype_for_size <- function(size) {
    # R int32 caps at 2^31-1 = .Machine$integer.max. We conservatively pick
    # UInt32 only when the axis fits R's native int.
    if (size <= .Machine$integer.max) "UInt32" else "UInt64"
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
        sum(vec != 0, na.rm = TRUE)
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
