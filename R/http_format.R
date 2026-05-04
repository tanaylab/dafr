#' @include classes.R format_api.R cache_group.R
#' @importFrom utils unzip
NULL

#' Read-only HTTP-served FilesDaf.
#'
#' A `Daf` reader that fetches a [files_daf()] directory served over
#' HTTP(S). The server must expose the FilesDaf tree verbatim and include a
#' `metadata.zip` bundle at the root (see [pack_files_daf_metadata()]).
#' From dafr 0.2.0 onward, FilesDaf writes maintain `metadata.zip`
#' automatically; for pre-0.2.0 stores call `pack_files_daf_metadata()`
#' before publishing.
#'
#' The client downloads `metadata.zip` once at open time, parses it in
#' memory, and serves all JSON metadata from it (no further HTTP traffic
#' for `format_has_*` / `format_*_set` / scalar reads). Non-JSON payloads
#' (axis `.txt` files, vector/matrix `.data` / `.nzind` / `.nzval` /
#' `.colptr` / `.rowval` / `.nztxt`) are fetched lazily on first access via
#' one HTTP GET each, cached by the standard cache layer.
#'
#' Read-only — mutations are not supported. Server data is assumed stable
#' while a [HttpDaf] is open; reopen to pick up changes.
#'
#' @section HTTP timeout:
#' Each GET uses a 30-second timeout. Override via
#' `options(dafr.http_timeout = N)` (seconds) or environment variable
#' `DAFR_HTTP_TIMEOUT` for slow links or large lazy payloads. Requests
#' are issued without retry; flaky-network handling is on the caller.
#'
#' @section URL-derived name:
#' When neither `name` nor a `name` scalar in the daf is set, the
#' default name is `basename(url)`. Two HttpDafs opened from
#' different hosts but the same basename will collide on name; pass an
#' explicit `name` to disambiguate.
#'
#' @param url HTTP(S) URL pointing at a FilesDaf root.
#' @param name Optional override; defaults to the daf's `name` scalar (if
#'   any) or the URL basename.
#' @inheritParams DafReader
#' @return A `HttpDaf` instance (`DafReadOnly` subclass).
#' @examples
#' \dontrun{
#' d <- http_daf("https://example.com/path/to/data.daf/")
#' get_scalar(d, "organism")
#' }
#' @export
HttpDaf <- S7::new_class(
    name    = "HttpDaf",
    package = "dafr",
    parent  = DafReadOnly
)

#' @rdname HttpDaf
#' @export
http_daf <- function(url, name = NULL) {
    stopifnot(is.character(url), length(url) == 1L, !is.na(url))
    if (!grepl("^https?://", url)) {
        stop(sprintf("not an HTTP(S) URL: %s", url), call. = FALSE)
    }
    url <- sub("/+$", "", url)

    zip_bytes <- .dafr_http_get(paste0(url, "/metadata.zip"))
    zip_path <- tempfile("dafr-http-meta-", fileext = ".zip")
    writeBin(zip_bytes, zip_path)

    zip_names_full <- unzip(zip_path, list = TRUE)$Name
    if (!"daf.json" %in% zip_names_full) {
        unlink(zip_path, force = TRUE)
        stop(sprintf("not a daf data set: %s", url), call. = FALSE)
    }
    daf_json <- jsonlite::fromJSON(unz(zip_path, "daf.json"),
                                   simplifyVector = TRUE)
    v <- daf_json$version
    if (length(v) != 2L || v[[1]] != 1L || v[[2]] > 0L) {
        unlink(zip_path, force = TRUE)
        stop(sprintf(paste0("incompatible format version: %d.%d\n",
                            "for the daf HTTP data set: %s\n",
                            "the code supports version: 1.0"),
                     v[[1]], v[[2]], url), call. = FALSE)
    }

    if (is.null(name)) {
        if ("scalars/name.json" %in% zip_names_full) {
            j <- jsonlite::fromJSON(unz(zip_path, "scalars/name.json"),
                                    simplifyVector = TRUE)
            name <- as.character(j$value)
        } else {
            # URL-derived default: basename strips the scheme + path so the
            # result passes .assert_name's forbidden-character check
            # (which rejects '/', ':'). Mirrors upstream's unique_name(url)
            # at the level of "human-readable defaults".
            name <- basename(url)
            if (!nzchar(name)) name <- "http"
        }
    }
    .assert_name(name, "name")

    internal <- new_internal_env()
    internal$url <- url
    internal$path <- url      # so complete_path(daf) returns the URL
    internal$zip_path <- zip_path
    internal$zip_names <- zip_names_full
    internal$is_frozen <- TRUE

    # Clean up the temp zip when the internal env is GC'd.
    reg.finalizer(internal, function(e) {
        zp <- e$zip_path
        if (!is.null(zp) && file.exists(zp)) unlink(zp, force = TRUE)
    }, onexit = TRUE)

    HttpDaf(
        name                   = name,
        internal               = internal,
        cache                  = new_cache_env(),
        axis_version_counter   = new_counter_env(),
        vector_version_counter = new_counter_env(),
        matrix_version_counter = new_counter_env()
    )
}

# ---- Helpers ---------------------------------------------------------------

.http_zip_path  <- function(daf) S7::prop(daf, "internal")$zip_path
.http_zip_names <- function(daf) S7::prop(daf, "internal")$zip_names
.http_url       <- function(daf) S7::prop(daf, "internal")$url

.http_zip_json <- function(daf, relative_path) {
    jsonlite::fromJSON(unz(.http_zip_path(daf), relative_path),
                       simplifyVector = TRUE)
}

# Split HTTP-fetched text bytes into lines, dropping the trailing empty
# from a final newline. Mirrors upstream fetch_lines.
.http_split_text_lines <- function(bytes) {
    text <- rawToChar(bytes)
    lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
    if (length(lines) > 0L && lines[[length(lines)]] == "") {
        lines <- lines[-length(lines)]
    }
    lines
}

# Decode a parsed scalar JSON descriptor (`list($type, $value)` from
# jsonlite::fromJSON simplifyVector=TRUE) into the typed R value.
# Mirrors files_io.R's .read_scalar_json fallback path.
.http_scalar_from_json <- function(j) {
    type <- j$type
    val  <- j$value
    if (is.null(type)) {
        stop("HttpDaf: scalar JSON missing $type", call. = FALSE)
    }
    t <- .dtype_canonical(type)
    switch(t,
        String  = as.character(val),
        Bool    = as.logical(as.integer(val)),
        Int8    = ,
        Int16   = ,
        Int32   = as.integer(val),
        Int64   = bit64::as.integer64(val),
        UInt8   = ,
        UInt16  = ,
        UInt32  = as.integer(val),
        UInt64  = bit64::as.integer64(val),
        Float32 = as.double(val),
        Float64 = as.double(val),
        stop(sprintf("HttpDaf: unknown scalar dtype %s", sQuote(type)), call. = FALSE)
    )
}

# ---- Scalars ---------------------------------------------------------------

S7::method(
    format_has_scalar,
    list(HttpDaf, S7::class_character)
) <- function(daf, name) {
    paste0("scalars/", name, ".json") %in% .http_zip_names(daf)
}

S7::method(
    format_get_scalar,
    list(HttpDaf, S7::class_character)
) <- function(daf, name) {
    j <- .http_zip_json(daf, paste0("scalars/", name, ".json"))
    .cache_group_value(.http_scalar_from_json(j), MEMORY_DATA)
}

S7::method(
    format_scalars_set,
    HttpDaf
) <- function(daf) {
    names <- .http_zip_names(daf)
    pattern <- "^scalars/([^/]+)\\.json$"
    matches <- regmatches(names, regexec(pattern, names))
    out <- vapply(matches,
                  function(m) if (length(m) == 2L) m[[2L]] else NA_character_,
                  character(1L))
    sort(out[!is.na(out)])
}

# ---- Axes ------------------------------------------------------------------

S7::method(
    format_has_axis,
    list(HttpDaf, S7::class_character)
) <- function(daf, axis) {
    axis %in% format_axes_set(daf)
}

S7::method(
    format_axes_set,
    HttpDaf
) <- function(daf) {
    if (!"axes/metadata.json" %in% .http_zip_names(daf)) {
        return(character(0L))
    }
    j <- .http_zip_json(daf, "axes/metadata.json")
    # jsonlite::fromJSON("[]", simplifyVector = TRUE) returns list(); coerce.
    if (is.list(j) && length(j) == 0L) return(character(0L))
    sort(as.character(j))
}

S7::method(
    format_axis_array,
    list(HttpDaf, S7::class_character)
) <- function(daf, axis) {
    bytes <- .dafr_http_get(paste0(.http_url(daf), "/axes/", axis, ".txt"))
    lines <- .http_split_text_lines(bytes)
    .cache_group_value(lines, MEMORY_DATA)
}

S7::method(
    format_axis_dict,
    list(HttpDaf, S7::class_character)
) <- function(daf, axis) {
    entries <- format_axis_array(daf, axis)$value
    dict <- new.env(parent = emptyenv(), size = length(entries))
    for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
    dict
}

S7::method(
    format_axis_length,
    list(HttpDaf, S7::class_character)
) <- function(daf, axis) {
    length(format_axis_array(daf, axis)$value)
}

# ---- Vectors / matrices: helpers -------------------------------------------

# In-memory equivalent of files_io.R::.read_bin_dense. Reads `n` elements
# of `dtype` from a raw byte vector.
.http_bytes_to_dense <- function(bytes, dtype, n) {
    dtype <- .dtype_canonical(dtype)
    if (length(bytes) < n * .dtype_size(dtype)) {
        stop(sprintf("HttpDaf: payload truncated (%d < %d bytes for dtype %s, n=%d)",
                     length(bytes), n * .dtype_size(dtype), dtype, n),
             call. = FALSE)
    }
    con <- rawConnection(bytes, open = "rb")
    on.exit(close(con), add = TRUE)
    switch(dtype,
        Bool    = as.logical(readBin(con, what = "integer", n = n, size = 1L,
                                     signed = FALSE, endian = "little")),
        Int8    = readBin(con, what = "integer", n = n, size = 1L, signed = TRUE,  endian = "little"),
        Int16   = readBin(con, what = "integer", n = n, size = 2L, signed = TRUE,  endian = "little"),
        Int32   = readBin(con, what = "integer", n = n, size = 4L, signed = TRUE,  endian = "little"),
        Int64   = bit64::as.integer64(readBin(con, what = "integer", n = n, size = 8L, endian = "little")),
        UInt8   = readBin(con, what = "integer", n = n, size = 1L, signed = FALSE, endian = "little"),
        UInt16  = readBin(con, what = "integer", n = n, size = 2L, signed = FALSE, endian = "little"),
        UInt32  = readBin(con, what = "integer", n = n, size = 4L, signed = TRUE,  endian = "little"),
        UInt64  = bit64::as.integer64(readBin(con, what = "integer", n = n, size = 8L, endian = "little")),
        Float32 = readBin(con, what = "double",  n = n, size = 4L, endian = "little"),
        Float64 = readBin(con, what = "double",  n = n, size = 8L, endian = "little"),
        stop(sprintf("HttpDaf: unsupported dtype %s for dense read", dtype),
             call. = FALSE)
    )
}

# Empty typed vector of length n for sparse reconstruction.
.http_zero_vector <- function(eltype, n) {
    eltype <- .dtype_canonical(eltype)
    if (eltype == "Bool") return(logical(n))
    if (eltype %in% c("Int8", "Int16", "Int32", "UInt8", "UInt16", "UInt32")) {
        return(integer(n))
    }
    if (eltype %in% c("Int64", "UInt64")) {
        return(bit64::as.integer64(integer(n)))
    }
    if (eltype %in% c("Float32", "Float64")) return(numeric(n))
    stop(sprintf("HttpDaf: unsupported eltype %s", eltype), call. = FALSE)
}

# Decode a vector descriptor JSON (parsed) into canonical fields. Mirrors
# files_io.R::.read_descriptor's output shape.
.http_descriptor <- function(j) {
    fmt <- j$format
    if (is.null(fmt) || !(fmt %in% c("dense", "sparse"))) {
        stop("HttpDaf: descriptor missing $format", call. = FALSE)
    }
    list(
        format  = fmt,
        eltype  = .dtype_canonical(j$eltype),
        indtype = if (is.null(j$indtype)) NULL else .dtype_canonical(j$indtype)
    )
}

# ---- Vectors ---------------------------------------------------------------

S7::method(
    format_has_vector,
    list(HttpDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    paste0("vectors/", axis, "/", name, ".json") %in% .http_zip_names(daf)
}

S7::method(
    format_vectors_set,
    list(HttpDaf, S7::class_character)
) <- function(daf, axis) {
    names <- .http_zip_names(daf)
    pattern <- paste0("^vectors/", axis, "/([^/]+)\\.json$")
    matches <- regmatches(names, regexec(pattern, names))
    out <- vapply(matches,
                  function(m) if (length(m) == 2L) m[[2L]] else NA_character_,
                  character(1L))
    sort(out[!is.na(out)])
}

S7::method(
    format_get_vector,
    list(HttpDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    desc <- .http_descriptor(.http_zip_json(
        daf, paste0("vectors/", axis, "/", name, ".json")
    ))
    n <- format_axis_length(daf, axis)
    base <- paste0(.http_url(daf), "/vectors/", axis, "/", name)

    if (desc$format == "dense") {
        if (desc$eltype == "String") {
            bytes <- .dafr_http_get(paste0(base, ".txt"))
            lines <- .http_split_text_lines(bytes)
            if (length(lines) != n) {
                stop(sprintf("HttpDaf: string vector %s has %d entries (expected %d)",
                             sQuote(name), length(lines), n), call. = FALSE)
            }
            return(.cache_group_value(
                .attach_vector_axis_names(daf, axis, lines), MEMORY_DATA))
        }
        bytes <- .dafr_http_get(paste0(base, ".data"))
        v <- .http_bytes_to_dense(bytes, desc$eltype, n)
        return(.cache_group_value(
            .attach_vector_axis_names(daf, axis, v), MEMORY_DATA))
    }

    # sparse
    indtype <- desc$indtype %||% "UInt32"
    nzind_bytes <- .dafr_http_get(paste0(base, ".nzind"))
    nnz <- length(nzind_bytes) %/% .dtype_size(indtype)
    idx <- .http_bytes_to_dense(nzind_bytes, indtype, nnz)

    if (desc$eltype == "String") {
        nztxt_bytes <- .dafr_http_get(paste0(base, ".nztxt"))
        vals <- .http_split_text_lines(nztxt_bytes)
        if (length(vals) != nnz) {
            stop(sprintf("HttpDaf: sparse string vector %s .nztxt has %d lines (expected %d)",
                         sQuote(name), length(vals), nnz), call. = FALSE)
        }
        out <- rep("", n)
        out[as.integer(idx)] <- vals
        return(.cache_group_value(
            .attach_vector_axis_names(daf, axis, out), MEMORY_DATA))
    }
    if (desc$eltype == "Bool") {
        nzval_bytes <- .dafr_http_get(paste0(base, ".nzval"), allow_404 = TRUE)
        out <- logical(n)
        if (is.null(nzval_bytes)) {
            # All-true Bool optimization: .nzval absent → every nzind is TRUE.
            out[as.integer(idx)] <- TRUE
        } else {
            vals <- as.logical(.http_bytes_to_dense(nzval_bytes, "Bool", nnz))
            out[as.integer(idx)] <- vals
        }
        return(.cache_group_value(
            .attach_vector_axis_names(daf, axis, out), MEMORY_DATA))
    }
    nzval_bytes <- .dafr_http_get(paste0(base, ".nzval"))
    vals <- .http_bytes_to_dense(nzval_bytes, desc$eltype, nnz)
    out <- .http_zero_vector(desc$eltype, n)
    out[as.integer(idx)] <- vals
    .cache_group_value(.attach_vector_axis_names(daf, axis, out), MEMORY_DATA)
}

# ---- Matrices --------------------------------------------------------------

S7::method(
    format_has_matrix,
    list(HttpDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    paste0("matrices/", rows_axis, "/", columns_axis, "/", name, ".json") %in%
        .http_zip_names(daf)
}

S7::method(
    format_matrices_set,
    list(HttpDaf, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis) {
    names <- .http_zip_names(daf)
    pattern <- paste0("^matrices/", rows_axis, "/", columns_axis, "/([^/]+)\\.json$")
    matches <- regmatches(names, regexec(pattern, names))
    out <- vapply(matches,
                  function(m) if (length(m) == 2L) m[[2L]] else NA_character_,
                  character(1L))
    sort(out[!is.na(out)])
}

S7::method(
    format_get_matrix,
    list(HttpDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    desc <- .http_descriptor(.http_zip_json(
        daf, paste0("matrices/", rows_axis, "/", columns_axis, "/", name, ".json")
    ))
    nr <- format_axis_length(daf, rows_axis)
    nc <- format_axis_length(daf, columns_axis)
    base <- paste0(.http_url(daf), "/matrices/", rows_axis, "/", columns_axis,
                   "/", name)

    if (desc$format == "dense") {
        if (desc$eltype == "String") {
            bytes <- .dafr_http_get(paste0(base, ".txt"))
            vals <- .http_split_text_lines(bytes)
            expected <- nr * nc
            if (length(vals) != expected) {
                stop(sprintf("HttpDaf: string matrix has %d lines (expected %d)",
                             length(vals), expected), call. = FALSE)
            }
            return(.cache_group_value(
                .attach_matrix_axis_dimnames(daf, rows_axis, columns_axis,
                    matrix(vals, nrow = nr, ncol = nc)),
                MEMORY_DATA))
        }
        bytes <- .dafr_http_get(paste0(base, ".data"))
        total <- as.integer(nr) * as.integer(nc)
        v <- .http_bytes_to_dense(bytes, desc$eltype, total)
        dim(v) <- c(as.integer(nr), as.integer(nc))
        return(.cache_group_value(
            .attach_matrix_axis_dimnames(daf, rows_axis, columns_axis, v),
            MEMORY_DATA))
    }

    # sparse — CSC layout
    indtype <- desc$indtype %||% "UInt32"
    colptr_bytes <- .dafr_http_get(paste0(base, ".colptr"))
    colptr <- .http_bytes_to_dense(colptr_bytes, indtype,
                                   as.integer(nc) + 1L)
    nnz <- as.integer(colptr[length(colptr)]) - 1L
    rowval <- if (nnz > 0L) {
        rowval_bytes <- .dafr_http_get(paste0(base, ".rowval"))
        .http_bytes_to_dense(rowval_bytes, indtype, nnz)
    } else {
        integer(0L)
    }

    if (desc$eltype == "Bool") {
        nzval_bytes <- .dafr_http_get(paste0(base, ".nzval"), allow_404 = TRUE)
        vals <- if (is.null(nzval_bytes)) {
            rep(TRUE, nnz)
        } else {
            as.logical(.http_bytes_to_dense(nzval_bytes, "Bool", nnz))
        }
        m <- methods::new("lgCMatrix",
            x = vals,
            i = as.integer(rowval) - 1L,
            p = as.integer(colptr) - 1L,
            Dim = c(as.integer(nr), as.integer(nc)),
            Dimnames = list(NULL, NULL)
        )
        return(.cache_group_value(
            .attach_matrix_axis_dimnames(daf, rows_axis, columns_axis, m),
            MEMORY_DATA))
    }

    if (desc$eltype == "String") {
        # Sparse string matrices: scatter into a dense character matrix.
        # Mirrors upstream http_format.jl.
        nztxt_bytes <- .dafr_http_get(paste0(base, ".nztxt"))
        vals <- .http_split_text_lines(nztxt_bytes)
        m <- matrix("", nrow = nr, ncol = nc)
        position <- 1L
        colptr_i <- as.integer(colptr)
        rowval_i <- as.integer(rowval)
        for (col in seq_len(nc)) {
            first <- colptr_i[col]
            last  <- colptr_i[col + 1L] - 1L
            if (last >= first) {
                for (k in seq.int(first, last)) {
                    m[rowval_i[k], col] <- vals[position]
                    position <- position + 1L
                }
            }
        }
        return(.cache_group_value(
            .attach_matrix_axis_dimnames(daf, rows_axis, columns_axis, m),
            MEMORY_DATA))
    }

    nzval_bytes <- .dafr_http_get(paste0(base, ".nzval"))
    vals <- if (nnz > 0L) {
        .http_bytes_to_dense(nzval_bytes, desc$eltype, nnz)
    } else {
        double(0L)
    }
    m <- methods::new("dgCMatrix",
        x = as.double(vals),
        i = as.integer(rowval) - 1L,
        p = as.integer(colptr) - 1L,
        Dim = c(as.integer(nr), as.integer(nc)),
        Dimnames = list(NULL, NULL)
    )
    .cache_group_value(
        .attach_matrix_axis_dimnames(daf, rows_axis, columns_axis, m),
        MEMORY_DATA)
}

S7::method(
    format_relayout_matrix,
    list(HttpDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    stop("HttpDaf is read-only; cannot relayout. Fetch the transposed matrix directly.",
         call. = FALSE)
}

# ---- Description header --------------------------------------------------
# Upstream Julia Formats.format_description_header(::HttpDaf, ...) at
# http_format.jl:393 emits `type: HttpDaf` and `url: <url>`.
S7::method(format_description_header, HttpDaf) <- function(daf, indent = "",
                                                            deep = FALSE) {
    c(paste0(indent, "type: HttpDaf"),
      paste0(indent, "url: ", S7::prop(daf, "internal")$url))
}

# Upstream Julia Readers.is_leaf(::HttpDaf) at http_format.jl:185.
S7::method(is_leaf, HttpDaf) <- function(daf) TRUE
