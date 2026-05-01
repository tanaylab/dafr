#' @include classes.R format_api.R cache_group.R
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
#' @param url HTTP(S) URL pointing at a FilesDaf root.
#' @param name Optional override; defaults to the daf's `name` scalar (if
#'   any) or the URL.
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
            name <- url
        }
    }
    .assert_name(name, "name")

    internal <- new_internal_env()
    internal$url <- url
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
