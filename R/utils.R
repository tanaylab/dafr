`%||%` <- function(a, b) if (is.null(a)) b else a

.FORBIDDEN_NAME_CHARS <- "[/\\\\:,\n\r\t\\x00]"

# Scalar-character argument guard used by the user-facing wrappers in
# readers.R / writers.R. Kept here (not inlined via stopifnot) so error
# messaging can be tuned in one place.
.assert_name <- function(value, arg) {
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
        stop(sprintf("`%s` must be a non-NA character scalar", arg), call. = FALSE)
    }
    if (!nzchar(value)) {
        stop(sprintf("`%s` may not be empty", arg), call. = FALSE)
    }
    if (value != trimws(value)) {
        stop(sprintf(
            "`%s` may not have leading/trailing whitespace: %s",
            arg, sQuote(value)
        ), call. = FALSE)
    }
    if (grepl(.FORBIDDEN_NAME_CHARS, value, perl = TRUE)) {
        stop(sprintf("`%s` contains forbidden character(s): %s", arg, sQuote(value)),
            call. = FALSE
        )
    }
    invisible()
}

# Scalar-logical argument guard — used for overwrite / must_exist / etc.
.assert_flag <- function(value, arg) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
        stop(sprintf("`%s` must be a non-NA logical scalar", arg),
            call. = FALSE
        )
    }
    invisible()
}

.assert_scalar_value <- function(name, value) {
    if (is.null(value)) {
        stop(sprintf("scalar %s value may not be NULL", sQuote(name)), call. = FALSE)
    }
    if (!is.atomic(value)) {
        stop(sprintf("scalar %s value must be an atomic scalar", sQuote(name)), call. = FALSE)
    }
    if (length(value) != 1L) {
        stop(sprintf("scalar %s value must have length 1 (got %d)", sQuote(name), length(value)), call. = FALSE)
    }
    if (is.na(value)) {
        stop(sprintf("scalar %s value may not be NA", sQuote(name)), call. = FALSE)
    }
    invisible()
}

.validate_vector_value <- function(daf, axis, name, vec) {
    if (is.null(vec) || !is.atomic(vec)) {
        stop(sprintf("vector %s on axis %s must be atomic", sQuote(name), sQuote(axis)),
            call. = FALSE
        )
    }
    n <- format_axis_length(daf, axis)
    if (!is.null(names(vec))) {
        entries <- format_axis_array(daf, axis)$value
        missing <- setdiff(names(vec), entries)
        if (length(missing)) {
            stop(
                sprintf(
                    "vector %s has names not in axis %s: %s",
                    sQuote(name), sQuote(axis),
                    paste(sQuote(missing), collapse = ", ")
                ),
                call. = FALSE
            )
        }
        .require_axis_length(daf, length(vec), sprintf("vector: %s", name), axis)
        # Reorder to axis order; drop names but preserve class (e.g. bit64).
        vec <- vec[entries]
        names(vec) <- NULL
    } else {
        .require_axis_length(daf, length(vec), sprintf("vector: %s", name), axis)
    }
    vec
}

# Attach axis-entry names to a vector returned by format_get_vector.
# Internal — every format_get_vector method must call this on the value
# it returns so the format-API contract ("returns are named") holds at
# every layer (memory, files, zarr, http, chain, view, contract).
#
# The helper is length-strict: a backend that returns a value of the
# wrong length is buggy regardless of names, and we'd rather surface
# that immediately than silently mismatch names to data.
#
# Note: on main, format_axis_array returns a cache_group_value list, so
# we unpack `$value` to get the bare axis entries.
.attach_vector_axis_names <- function(daf, axis, vec) {
    entries <- format_axis_array(daf, axis)$value
    if (length(vec) != length(entries)) {
        stop(sprintf(
            "format_get_vector contract violation: value has length %d, axis %s has %d entries",
            length(vec), sQuote(axis), length(entries)
        ), call. = FALSE)
    }
    names(vec) <- entries
    vec
}

# Attach axis-entry dimnames to a matrix returned by format_get_matrix.
# Handles both base R dense matrices and Matrix::dgCMatrix /
# Matrix::lgCMatrix (which carry dimnames on the @Dimnames slot).
.attach_matrix_axis_dimnames <- function(daf, rows_axis, columns_axis, mat) {
    rows <- format_axis_array(daf, rows_axis)$value
    cols <- format_axis_array(daf, columns_axis)$value
    d <- dim(mat)
    if (d[[1L]] != length(rows) || d[[2L]] != length(cols)) {
        stop(sprintf(
            "format_get_matrix contract violation: matrix is %dx%d, axes (%s,%s) are %dx%d",
            d[[1L]], d[[2L]], sQuote(rows_axis), sQuote(columns_axis),
            length(rows), length(cols)
        ), call. = FALSE)
    }
    if (methods::is(mat, "dgCMatrix") || methods::is(mat, "lgCMatrix")) {
        mat@Dimnames <- list(rows, cols)
    } else {
        dimnames(mat) <- list(rows, cols)
    }
    mat
}
