#' @include classes.R format_api.R cache.R
NULL

#' Read-only chain of DafReaders.
#'
#' Produced by [chain_reader()]. Every `format_*` read falls through the
#' chain in reverse order (last wins); writes raise.
#' @inheritParams DafReader
#' @param dafs Ordered list of base `DafReader`s.
#' @export
ReadOnlyChainDaf <- S7::new_class(
    name = "ReadOnlyChainDaf",
    package = "dafr",
    parent = DafReadOnly,
    properties = list(dafs = S7::class_list)
)

#' Write chain of DafReaders with a final DafWriter.
#'
#' Produced by [chain_writer()]. Reads fall through in reverse order
#' (writer last-wins); writes go to the final writer; deletes succeed
#' only if the entry does not exist in any earlier daf.
#' @inheritParams DafReader
#' @param dafs Ordered list of base `DafReader`s.
#' @param writer The final `DafWriter` (== `dafs[[length(dafs)]]`).
#' @export
WriteChainDaf <- S7::new_class(
    name = "WriteChainDaf",
    package = "dafr",
    parent = DafWriter,
    properties = list(
        dafs   = S7::class_list,
        writer = DafWriter
    )
)

#' Create a read-only chain of DafReaders.
#'
#' @param dafs Ordered list of `DafReader`s. Later entries override earlier
#'   entries on read.
#' @param name Optional chain name; defaults to `paste(names, collapse = ";")`.
#' @return A `ReadOnlyChainDaf`.
#' @export
chain_reader <- function(dafs, name = NULL) {
    if (!is.list(dafs) || length(dafs) == 0L) {
        stop(sprintf("empty chain%s",
            if (is.null(name)) "" else paste0(": ", name)
        ), call. = FALSE)
    }
    for (d in dafs) {
        if (!S7::S7_inherits(d, DafReader)) {
            stop("chain entries must all be DafReaders", call. = FALSE)
        }
    }
    if (is.null(name)) {
        name <- paste(vapply(dafs, function(d) S7::prop(d, "name"), character(1)),
            collapse = ";"
        )
    }
    .validate_chain_axes(dafs, name)
    ReadOnlyChainDaf(
        name                   = name,
        internal               = new_internal_env(),
        cache                  = new_cache_env(),
        axis_version_counter   = new_counter_env(),
        vector_version_counter = new_counter_env(),
        matrix_version_counter = new_counter_env(),
        dafs                   = dafs
    )
}

#' Create a chain of DafReaders with a final DafWriter.
#' @inheritParams chain_reader
#' @return A `WriteChainDaf`.
#' @export
chain_writer <- function(dafs, name = NULL) {
    if (!is.list(dafs) || length(dafs) == 0L) {
        stop(sprintf("empty chain%s",
            if (is.null(name)) "" else paste0(": ", name)
        ), call. = FALSE)
    }
    for (d in dafs) {
        if (!S7::S7_inherits(d, DafReader)) {
            stop("chain entries must all be DafReaders", call. = FALSE)
        }
    }
    writer <- dafs[[length(dafs)]]
    if (!S7::S7_inherits(writer, DafWriter)) {
        stop(sprintf(
            "read-only final data: %s in write chain%s",
            S7::prop(writer, "name"),
            if (is.null(name)) "" else paste0(": ", name)
        ), call. = FALSE)
    }
    if (is.null(name)) {
        name <- paste(vapply(dafs, function(d) S7::prop(d, "name"), character(1)),
            collapse = ";"
        )
    }
    .validate_chain_axes(dafs, name)
    WriteChainDaf(
        name                   = name,
        internal               = new_internal_env(),
        cache                  = new_cache_env(),
        axis_version_counter   = new_counter_env(),
        vector_version_counter = new_counter_env(),
        matrix_version_counter = new_counter_env(),
        dafs                   = dafs,
        writer                 = writer
    )
}

.chain_dafs <- function(daf) S7::prop(daf, "dafs")

.chain_writer <- function(daf) S7::prop(daf, "writer")

S7::method(
    format_set_scalar,
    list(WriteChainDaf, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, name, value, overwrite) {
    format_set_scalar(.chain_writer(daf), name, value, overwrite)
}

S7::method(
    format_delete_scalar,
    list(WriteChainDaf, S7::class_character, S7::class_logical)
) <- function(daf, name, must_exist) {
    earlier <- .chain_dafs(daf)
    earlier <- earlier[-length(earlier)]  # all except writer
    for (d in rev(earlier)) {
        if (format_has_scalar(d, name)) {
            stop(sprintf(
                "failed to delete the scalar: %s from the daf data: %s of the chain: %s because it exists in the earlier: %s",
                name, S7::prop(.chain_writer(daf), "name"),
                S7::prop(daf, "name"), S7::prop(d, "name")
            ), call. = FALSE)
        }
    }
    format_delete_scalar(.chain_writer(daf), name, must_exist)
}

S7::method(
    format_has_scalar,
    list(ReadOnlyChainDaf, S7::class_character)
) <- function(daf, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_scalar(d, name)) return(TRUE)
    }
    FALSE
}

S7::method(
    format_get_scalar,
    list(ReadOnlyChainDaf, S7::class_character)
) <- function(daf, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_scalar(d, name)) return(format_get_scalar(d, name))
    }
    stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
}

S7::method(format_scalars_set, ReadOnlyChainDaf) <- function(daf) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        out <- c(out, format_scalars_set(d))
    }
    sort(unique(out), method = "radix")
}

S7::method(
    format_has_scalar,
    list(WriteChainDaf, S7::class_character)
) <- function(daf, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_scalar(d, name)) return(TRUE)
    }
    FALSE
}

S7::method(
    format_get_scalar,
    list(WriteChainDaf, S7::class_character)
) <- function(daf, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_scalar(d, name)) return(format_get_scalar(d, name))
    }
    stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
}

S7::method(format_scalars_set, WriteChainDaf) <- function(daf) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        out <- c(out, format_scalars_set(d))
    }
    sort(unique(out), method = "radix")
}

S7::method(
    format_has_axis,
    list(ReadOnlyChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(TRUE)
    }
    FALSE
}

S7::method(format_axes_set, ReadOnlyChainDaf) <- function(daf) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        out <- c(out, format_axes_set(d))
    }
    sort(unique(out), method = "radix")
}

S7::method(
    format_axis_array,
    list(ReadOnlyChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(format_axis_array(d, axis))
    }
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
}

S7::method(
    format_axis_length,
    list(ReadOnlyChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(format_axis_length(d, axis))
    }
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
}

S7::method(
    format_axis_dict,
    list(ReadOnlyChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(format_axis_dict(d, axis))
    }
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
}

S7::method(
    format_has_axis,
    list(WriteChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(TRUE)
    }
    FALSE
}

S7::method(format_axes_set, WriteChainDaf) <- function(daf) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        out <- c(out, format_axes_set(d))
    }
    sort(unique(out), method = "radix")
}

S7::method(
    format_axis_array,
    list(WriteChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(format_axis_array(d, axis))
    }
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
}

S7::method(
    format_axis_length,
    list(WriteChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(format_axis_length(d, axis))
    }
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
}

S7::method(
    format_axis_dict,
    list(WriteChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(format_axis_dict(d, axis))
    }
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
}

S7::method(
    format_has_vector,
    list(ReadOnlyChainDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis) && format_has_vector(d, axis, name)) return(TRUE)
    }
    FALSE
}

S7::method(
    format_get_vector,
    list(ReadOnlyChainDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis) && format_has_vector(d, axis, name)) {
            return(format_get_vector(d, axis, name))
        }
    }
    stop(sprintf(
        "vector %s does not exist on axis %s",
        sQuote(name), sQuote(axis)
    ), call. = FALSE)
}

S7::method(
    format_vectors_set,
    list(ReadOnlyChainDaf, S7::class_character)
) <- function(daf, axis) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        if (format_has_axis(d, axis)) {
            out <- c(out, format_vectors_set(d, axis))
        }
    }
    sort(unique(out), method = "radix")
}

S7::method(
    format_has_vector,
    list(WriteChainDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis) && format_has_vector(d, axis, name)) return(TRUE)
    }
    FALSE
}

S7::method(
    format_get_vector,
    list(WriteChainDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis) && format_has_vector(d, axis, name)) {
            return(format_get_vector(d, axis, name))
        }
    }
    stop(sprintf(
        "vector %s does not exist on axis %s",
        sQuote(name), sQuote(axis)
    ), call. = FALSE)
}

S7::method(
    format_vectors_set,
    list(WriteChainDaf, S7::class_character)
) <- function(daf, axis) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        if (format_has_axis(d, axis)) {
            out <- c(out, format_vectors_set(d, axis))
        }
    }
    sort(unique(out), method = "radix")
}

S7::method(
    format_has_matrix,
    list(ReadOnlyChainDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, rows_axis) &&
            format_has_axis(d, columns_axis) &&
            format_has_matrix(d, rows_axis, columns_axis, name)) return(TRUE)
    }
    FALSE
}

S7::method(
    format_get_matrix,
    list(ReadOnlyChainDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, rows_axis) &&
            format_has_axis(d, columns_axis) &&
            format_has_matrix(d, rows_axis, columns_axis, name)) {
            return(format_get_matrix(d, rows_axis, columns_axis, name))
        }
    }
    stop(sprintf(
        "matrix %s does not exist on axes (%s, %s)",
        sQuote(name), sQuote(rows_axis), sQuote(columns_axis)
    ), call. = FALSE)
}

S7::method(
    format_matrices_set,
    list(ReadOnlyChainDaf, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        if (format_has_axis(d, rows_axis) && format_has_axis(d, columns_axis)) {
            out <- c(out, format_matrices_set(d, rows_axis, columns_axis))
        }
    }
    sort(unique(out), method = "radix")
}

S7::method(
    format_has_matrix,
    list(WriteChainDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, rows_axis) &&
            format_has_axis(d, columns_axis) &&
            format_has_matrix(d, rows_axis, columns_axis, name)) return(TRUE)
    }
    FALSE
}

S7::method(
    format_get_matrix,
    list(WriteChainDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, rows_axis) &&
            format_has_axis(d, columns_axis) &&
            format_has_matrix(d, rows_axis, columns_axis, name)) {
            return(format_get_matrix(d, rows_axis, columns_axis, name))
        }
    }
    stop(sprintf(
        "matrix %s does not exist on axes (%s, %s)",
        sQuote(name), sQuote(rows_axis), sQuote(columns_axis)
    ), call. = FALSE)
}

S7::method(
    format_matrices_set,
    list(WriteChainDaf, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        if (format_has_axis(d, rows_axis) && format_has_axis(d, columns_axis)) {
            out <- c(out, format_matrices_set(d, rows_axis, columns_axis))
        }
    }
    sort(unique(out), method = "radix")
}

.validate_chain_axes <- function(dafs, chain_name) {
    seen <- list()   # axis -> list(daf_name, entries)
    for (d in dafs) {
        dname <- S7::prop(d, "name")
        for (axis in format_axes_set(d)) {
            entries <- format_axis_array(d, axis)
            prior <- seen[[axis]]
            if (is.null(prior)) {
                seen[[axis]] <- list(name = dname, entries = entries)
                next
            }
            if (length(entries) != length(prior$entries)) {
                stop(sprintf(
                    "different number of entries: %d for the axis: %s in the daf data: %s from the number of entries: %d for the axis: %s in the daf data: %s in the chain: %s",
                    length(entries), axis, dname,
                    length(prior$entries), axis, prior$name, chain_name
                ), call. = FALSE)
            }
            mismatch <- which(entries != prior$entries)
            if (length(mismatch)) {
                i <- mismatch[[1L]]
                stop(sprintf(
                    "different entry#%d: %s for the axis: %s in the daf data: %s from the entry#%d: %s for the axis: %s in the daf data: %s in the chain: %s",
                    i, entries[[i]], axis, dname,
                    i, prior$entries[[i]], axis, prior$name, chain_name
                ), call. = FALSE)
            }
        }
    }
    invisible()
}
