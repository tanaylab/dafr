#' @include classes.R readers.R writers.R memory_daf.R chain_daf.R view_daf.R
NULL

# Internal undef sentinel — distinguishes "no default given" from "default = NULL".
.DAFR_UNDEF <- structure(list(), class = "dafr_undef")

.is_undef <- function(x) inherits(x, "dafr_undef")

# Coerce a scalar to a specified R storage type string.
# Supported type names: "logical", "integer", "double", "numeric", "character".
.cast_scalar <- function(value, type) {
    if (is.null(type)) return(value)
    if (!is.character(type) || length(type) != 1L) {
        stop("`type` must be a single string name", call. = FALSE)
    }
    switch(type,
        logical   = as.logical(value),
        integer   = as.integer(value),
        double    = ,
        numeric   = as.numeric(value),
        character = as.character(value),
        stop(sprintf("unsupported scalar type: %s", type), call. = FALSE)
    )
}

#' Copy a scalar from one daf to another.
#'
#' Mirrors Julia `copy_scalar!(; destination, source, name, rename, type,
#' default, overwrite, insist)`.
#'
#' @param destination A `DafWriter`.
#' @param source A `DafReader`.
#' @param name Name of the scalar in `source`.
#' @param rename If non-NULL, store under this name in `destination`.
#' @param type If non-NULL, coerce to this R storage type string
#'   (`"logical"`, `"integer"`, `"double"`, `"numeric"`, `"character"`).
#' @param default If unspecified, missing source raises. If `NULL`, missing
#'   source silently skips. Else, the value is used when source is absent.
#' @param overwrite If `TRUE`, replace an existing destination scalar.
#' @param insist If `TRUE` (default) and the destination already has the
#'   scalar, raise; if `FALSE`, silently skip.
#' @return Invisibly, the destination.
#' @export
#' @examples
#' src <- memory_daf(name = "src")
#' dest <- memory_daf(name = "dest")
#' set_scalar(src, "organism", "human")
#' copy_scalar(dest, src, "organism", rename = "species")
#' get_scalar(dest, "species")
copy_scalar <- function(destination, source, name,
                        rename = NULL, type = NULL,
                        default = .DAFR_UNDEF,
                        overwrite = FALSE, insist = TRUE) {
    .assert_name(name, "name")
    if (!is.null(rename)) .assert_name(rename, "rename")
    .assert_flag(overwrite, "overwrite")
    .assert_flag(insist, "insist")
    final_name <- if (is.null(rename)) name else rename
    if (format_has_scalar(destination, final_name) && !overwrite) {
        if (insist) {
            stop(sprintf("scalar %s already exists in destination",
                         sQuote(final_name)), call. = FALSE)
        }
        return(invisible(destination))
    }
    if (format_has_scalar(source, name)) {
        value <- format_get_scalar(source, name)
    } else if (.is_undef(default)) {
        stop(sprintf("missing scalar: %s in the daf data: %s",
                     sQuote(name), S7::prop(source, "name")),
             call. = FALSE)
    } else if (is.null(default)) {
        return(invisible(destination))
    } else {
        value <- default
    }
    value <- .cast_scalar(value, type)
    format_set_scalar(destination, final_name, value, overwrite = overwrite)
    invisible(destination)
}

#' Copy an axis (its entries) from one daf to another.
#'
#' Mirrors Julia `copy_axis!(; destination, source, axis, rename, overwrite,
#' insist)`.
#'
#' @param destination A `DafWriter`.
#' @param source A `DafReader`.
#' @param axis Axis name in `source`.
#' @param rename If non-NULL, use this name in `destination`.
#' @param overwrite If `TRUE`, delete any existing destination axis (and all
#'   its properties) before recreating.
#' @param insist If `TRUE` (default) and the destination already has the axis,
#'   raise; if `FALSE`, silently skip.
#' @return Invisibly, the destination.
#' @export
#' @examples
#' src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1", "c2"))
#' dest <- memory_daf(name = "dest")
#' copy_axis(dest, src, "cell")
copy_axis <- function(destination, source, axis,
                      rename = NULL, overwrite = FALSE, insist = TRUE) {
    .assert_name(axis, "axis")
    if (!is.null(rename)) .assert_name(rename, "rename")
    .assert_flag(overwrite, "overwrite")
    .assert_flag(insist, "insist")
    final_axis <- if (is.null(rename)) axis else rename
    if (!format_has_axis(source, axis)) {
        stop(sprintf("missing axis: %s in the daf data: %s",
                     sQuote(axis), S7::prop(source, "name")),
             call. = FALSE)
    }
    if (format_has_axis(destination, final_axis)) {
        if (!overwrite) {
            if (insist) {
                stop(sprintf("axis %s already exists in destination",
                             sQuote(final_axis)), call. = FALSE)
            }
            return(invisible(destination))
        }
        format_delete_axis(destination, final_axis, must_exist = TRUE)
    }
    format_add_axis(destination, final_axis, format_axis_array(source, axis))
    invisible(destination)
}

# Coerce a vector to a specified R storage type.
.cast_vector_type <- function(vec, type) {
    if (is.null(type)) return(vec)
    switch(type,
        logical   = as.logical(vec),
        integer   = as.integer(vec),
        double    = ,
        numeric   = as.numeric(vec),
        character = as.character(vec),
        stop(sprintf("unsupported vector type: %s", type), call. = FALSE)
    )
}

#' Copy a vector from one daf to another.
#'
#' Mirrors Julia `copy_vector!(; destination, source, axis, name, reaxis,
#' rename, eltype, default, empty, overwrite, insist)`.
#'
#' @param destination A `DafWriter`.
#' @param source A `DafReader`.
#' @param axis Axis name in `source`.
#' @param name Vector name in `source`.
#' @param rename If non-NULL, store under this name in `destination`.
#' @param reaxis If non-NULL, store on this (already-existing) destination axis.
#' @param type If non-NULL, coerce to this storage type string.
#' @param default If unspecified, missing source raises. If `NULL`, missing
#'   source silently skips. Else, a scalar (filled into every entry) or
#'   vector used when source is absent.
#' @param empty Value filled for destination-axis entries not present in the
#'   source axis (required when source axis is a subset of destination).
#' @param overwrite,insist See [copy_scalar()].
#' @return Invisibly, the destination.
#' @export
#' @examples
#' src <- memory_daf(name = "src")
#' add_axis(src, "cell", c("c1", "c2"))
#' set_vector(src, "cell", "age", c(10L, 20L))
#' dest <- memory_daf(name = "dest")
#' add_axis(dest, "cell", c("c1", "c2"))
#' copy_vector(dest, src, "cell", "age")
copy_vector <- function(destination, source, axis, name,
                        rename = NULL, reaxis = NULL, type = NULL,
                        default = .DAFR_UNDEF, empty = NULL,
                        overwrite = FALSE, insist = TRUE) {
    .assert_name(axis, "axis")
    .assert_name(name, "name")
    if (!is.null(rename)) .assert_name(rename, "rename")
    if (!is.null(reaxis)) .assert_name(reaxis, "reaxis")
    .assert_flag(overwrite, "overwrite")
    .assert_flag(insist, "insist")
    final_axis <- if (is.null(reaxis)) axis else reaxis
    final_name <- if (is.null(rename)) name else rename

    if (!format_has_axis(destination, final_axis)) {
        stop(sprintf("missing axis: %s in the destination daf data: %s",
                     sQuote(final_axis), S7::prop(destination, "name")),
             call. = FALSE)
    }
    if (format_has_vector(destination, final_axis, final_name) && !overwrite) {
        if (insist) {
            stop(sprintf("vector %s already exists on axis %s in destination",
                         sQuote(final_name), sQuote(final_axis)),
                 call. = FALSE)
        }
        return(invisible(destination))
    }

    # Fetch source value or resolve default.
    if (format_has_vector(source, axis, name)) {
        value <- format_get_vector(source, axis, name)
    } else if (.is_undef(default)) {
        stop(sprintf(
            "missing vector: %s for the axis: %s in the daf data: %s",
            sQuote(name), sQuote(axis), S7::prop(source, "name")
        ), call. = FALSE)
    } else if (is.null(default)) {
        return(invisible(destination))
    } else {
        # Expand scalar default to the full source-axis length; vector
        # defaults are used as-is.
        src_len <- format_axis_length(source, axis)
        value <- if (length(default) == 1L) rep(default, src_len) else default
    }

    relation <- .verify_axis_relation(source, axis, destination, final_axis)
    dest_entries <- format_axis_array(destination, final_axis)

    if (identical(relation, "same")) {
        out <- value
    } else if (identical(relation, "destination_is_subset")) {
        src_entries <- format_axis_array(source, axis)
        idx <- match(dest_entries, src_entries)
        out <- value[idx]
    } else if (identical(relation, "source_is_subset")) {
        if (is.null(empty)) {
            stop(sprintf(
                "missing entries in the axis: %s of the source daf %s which are needed for copying the vector: %s; supply `empty` to fill them",
                sQuote(axis), S7::prop(source, "name"), sQuote(name)
            ), call. = FALSE)
        }
        src_entries <- format_axis_array(source, axis)
        out <- rep(empty, length(dest_entries))
        idx <- match(src_entries, dest_entries)
        out[idx] <- value
    }
    out <- .cast_vector_type(out, type)
    format_set_vector(destination, final_axis, final_name, out,
                      overwrite = overwrite)
    invisible(destination)
}

# Detect the relation between a source axis and a destination axis.
# Returns one of: "same", "destination_is_subset", "source_is_subset".
# Raises for disjoint / partially-overlapping (non-subset) axes.
.verify_axis_relation <- function(source, source_axis, destination, dest_axis) {
    src_entries <- format_axis_array(source, source_axis)
    dest_entries <- format_axis_array(destination, dest_axis)
    if (length(src_entries) == length(dest_entries) &&
        identical(src_entries, dest_entries)) {
        return("same")
    }
    if (all(dest_entries %in% src_entries)) {
        return("destination_is_subset")
    }
    if (all(src_entries %in% dest_entries)) {
        return("source_is_subset")
    }
    stop(sprintf(
        "disjoint entries in the axis: source axis %s in %s and destination axis %s in %s",
        sQuote(source_axis), S7::prop(source, "name"),
        sQuote(dest_axis), S7::prop(destination, "name")
    ), call. = FALSE)
}

#' Copy a matrix from one daf to another.
#'
#' Mirrors Julia `copy_matrix!(; destination, source, rows_axis,
#' columns_axis, name, rows_reaxis, columns_reaxis, rename, eltype, default,
#' empty, relayout, overwrite, insist)`.
#'
#' @param destination A `DafWriter`.
#' @param source A `DafReader`.
#' @param rows_axis,columns_axis Axis names in `source`.
#' @param name Matrix name in `source`.
#' @param rows_reaxis,columns_reaxis If non-NULL, store on these
#'   destination axes (axes must already exist in `destination`).
#' @param rename If non-NULL, store under this name.
#' @param type If non-NULL, coerce to this storage type string.
#' @param default If unspecified, missing source raises. If `NULL`, silently
#'   skips. Else scalar filled into full source-shape matrix.
#' @param empty Value filled for entries whose row or column is missing in
#'   source but present in destination.
#' @param relayout If `TRUE` (default), also write the transposed layout.
#' @param overwrite,insist See [copy_scalar()].
#' @return Invisibly, the destination.
#' @export
#' @examples
#' src <- memory_daf(name = "src")
#' add_axis(src, "cell", c("c1", "c2"))
#' add_axis(src, "gene", c("g1", "g2"))
#' set_matrix(src, "cell", "gene", "UMIs",
#'            matrix(1:4, nrow = 2,
#'                   dimnames = list(c("c1","c2"), c("g1","g2"))))
#' dest <- memory_daf(name = "dest")
#' add_axis(dest, "cell", c("c1", "c2"))
#' add_axis(dest, "gene", c("g1", "g2"))
#' copy_matrix(dest, src, "cell", "gene", "UMIs", relayout = FALSE)
copy_matrix <- function(destination, source,
                        rows_axis, columns_axis, name,
                        rows_reaxis = NULL, columns_reaxis = NULL,
                        rename = NULL, type = NULL,
                        default = .DAFR_UNDEF, empty = NULL,
                        relayout = TRUE, overwrite = FALSE, insist = TRUE) {
    .assert_name(rows_axis, "rows_axis")
    .assert_name(columns_axis, "columns_axis")
    .assert_name(name, "name")
    if (!is.null(rows_reaxis)) .assert_name(rows_reaxis, "rows_reaxis")
    if (!is.null(columns_reaxis)) .assert_name(columns_reaxis, "columns_reaxis")
    if (!is.null(rename)) .assert_name(rename, "rename")
    .assert_flag(relayout, "relayout")
    .assert_flag(overwrite, "overwrite")
    .assert_flag(insist, "insist")
    final_rows <- if (is.null(rows_reaxis)) rows_axis else rows_reaxis
    final_cols <- if (is.null(columns_reaxis)) columns_axis else columns_reaxis
    final_name <- if (is.null(rename)) name else rename

    if (!format_has_axis(destination, final_rows)) {
        stop(sprintf("missing axis: %s in destination", sQuote(final_rows)),
             call. = FALSE)
    }
    if (!format_has_axis(destination, final_cols)) {
        stop(sprintf("missing axis: %s in destination", sQuote(final_cols)),
             call. = FALSE)
    }
    if (format_has_matrix(destination, final_rows, final_cols, final_name) &&
        !overwrite) {
        if (insist) {
            stop(sprintf(
                "matrix %s already exists on axes %s,%s in destination",
                sQuote(final_name), sQuote(final_rows), sQuote(final_cols)
            ), call. = FALSE)
        }
        return(invisible(destination))
    }

    # Resolve source matrix or default.
    if (format_has_matrix(source, rows_axis, columns_axis, name)) {
        value <- format_get_matrix(source, rows_axis, columns_axis, name)
    } else if (.is_undef(default)) {
        stop(sprintf(
            "missing matrix: %s for rows axis: %s and columns axis: %s in the daf data: %s",
            sQuote(name), sQuote(rows_axis), sQuote(columns_axis),
            S7::prop(source, "name")
        ), call. = FALSE)
    } else if (is.null(default)) {
        return(invisible(destination))
    } else {
        nr <- format_axis_length(source, rows_axis)
        nc <- format_axis_length(source, columns_axis)
        value <- matrix(default, nrow = nr, ncol = nc)
    }

    rows_rel <- .verify_axis_relation(source, rows_axis, destination, final_rows)
    cols_rel <- .verify_axis_relation(source, columns_axis, destination, final_cols)
    out <- .copy_matrix_with_relations(
        value = value,
        source = source, rows_axis = rows_axis, columns_axis = columns_axis,
        destination = destination, final_rows = final_rows, final_cols = final_cols,
        rows_rel = rows_rel, cols_rel = cols_rel, empty = empty, name = name
    )
    out <- .cast_matrix_type(out, type)

    format_set_matrix(destination, final_rows, final_cols, final_name, out,
                      overwrite = overwrite)
    if (relayout && final_rows != final_cols) {
        format_relayout_matrix(destination, final_rows, final_cols, final_name)
    }
    invisible(destination)
}

.cast_matrix_type <- function(m, type) {
    if (is.null(type)) return(m)
    switch(type,
        logical = {
            if (inherits(m, "dgCMatrix")) as.matrix(m) > 0 else as.logical(m)
        },
        integer = {
            d <- if (inherits(m, "dgCMatrix")) as.matrix(m) else m
            storage.mode(d) <- "integer"
            d
        },
        double = , numeric = {
            if (inherits(m, "dgCMatrix")) m else { storage.mode(m) <- "double"; m }
        },
        character = as.character(m),
        stop(sprintf("unsupported matrix type: %s", type), call. = FALSE)
    )
}

# Given a source value and the row/col relations, produce the final
# destination-shaped matrix. Sparse-preserving in pad modes.
.copy_matrix_with_relations <- function(value,
                                        source, rows_axis, columns_axis,
                                        destination, final_rows, final_cols,
                                        rows_rel, cols_rel, empty, name) {
    src_rows <- format_axis_array(source, rows_axis)
    src_cols <- format_axis_array(source, columns_axis)
    dest_rows <- format_axis_array(destination, final_rows)
    dest_cols <- format_axis_array(destination, final_cols)

    if (identical(rows_rel, "same") && identical(cols_rel, "same")) {
        return(value)
    }
    if ((rows_rel %in% c("same", "destination_is_subset")) &&
        (cols_rel %in% c("same", "destination_is_subset"))) {
        r_idx <- match(dest_rows, src_rows)
        c_idx <- match(dest_cols, src_cols)
        return(value[r_idx, c_idx, drop = FALSE])
    }
    if (is.null(empty)) {
        stop(sprintf(
            "missing entries in an axis of the source daf which are needed for copying the matrix: %s; supply `empty` to fill them",
            sQuote(name)
        ), call. = FALSE)
    }
    .embed_matrix_in_pad(value, src_rows, src_cols,
                         dest_rows, dest_cols, empty)
}

# Embed a source-shape matrix into a destination-shape matrix filled with
# `empty`. Sparse-preserving: if value is dgCMatrix and empty == 0, returns
# a dgCMatrix via Matrix::sparseMatrix; else builds dense.
.embed_matrix_in_pad <- function(value, src_rows, src_cols,
                                 dest_rows, dest_cols, empty) {
    n_dr <- length(dest_rows)
    n_dc <- length(dest_cols)
    r_map <- match(src_rows, dest_rows)
    c_map <- match(src_cols, dest_cols)
    keep_r <- !is.na(r_map)
    keep_c <- !is.na(c_map)
    r_map <- r_map[keep_r]
    c_map <- c_map[keep_c]

    if (inherits(value, "dgCMatrix") && isTRUE(empty == 0)) {
        v_sub <- value[keep_r, keep_c, drop = FALSE]
        tri <- Matrix::summary(as(v_sub, "TsparseMatrix"))
        return(Matrix::sparseMatrix(
            i = r_map[tri$i], j = c_map[tri$j], x = tri$x,
            dims = c(n_dr, n_dc),
            dimnames = list(dest_rows, dest_cols)
        ))
    }
    full <- matrix(empty, nrow = n_dr, ncol = n_dc,
                   dimnames = list(dest_rows, dest_cols))
    v_dense <- if (inherits(value, "dgCMatrix")) as.matrix(value[keep_r, keep_c, drop = FALSE]) else value[keep_r, keep_c, drop = FALSE]
    full[r_map, c_map] <- v_dense
    full
}

#' Copy a tensor (set of per-main-axis-entry matrices) between dafs.
#'
#' Mirrors Julia `copy_tensor!(; destination, source, main_axis, rows_axis,
#' columns_axis, name, rows_reaxis, columns_reaxis, rename, eltype, empty,
#' relayout, overwrite, insist)`.
#'
#' Iterates over `main_axis` entries in the destination. For each entry `E`,
#' copies the matrix named `"E_<name>"` (or `"E_<rename>"`) from source to
#' destination. If a per-entry source matrix is missing, `empty` is used as
#' the fill value. This supports a destination main axis that is a strict
#' superset of the source's.
#'
#' @param destination,source Daf data sets.
#' @param main_axis Axis whose entries define the per-matrix loop.
#' @param rows_axis,columns_axis Matrix row/column axes.
#' @param name Base name; full matrix name is `"<main_entry>_<name>"`.
#' @param rows_reaxis,columns_reaxis,rename,type,empty,relayout,overwrite,insist
#'   See [copy_matrix()].
#' @return Invisibly, the destination.
#' @export
#' @examples
#' src <- memory_daf(name = "src")
#' add_axis(src, "batch", c("b1", "b2"))
#' add_axis(src, "gene", c("g1"))
#' add_axis(src, "cell", c("c1"))
#' set_matrix(src, "gene", "cell", "b1_counts",
#'            matrix(1, 1, 1, dimnames = list("g1", "c1")))
#' set_matrix(src, "gene", "cell", "b2_counts",
#'            matrix(2, 1, 1, dimnames = list("g1", "c1")))
#' dest <- memory_daf(name = "dest")
#' add_axis(dest, "batch", c("b1", "b2"))
#' add_axis(dest, "gene", c("g1"))
#' add_axis(dest, "cell", c("c1"))
#' copy_tensor(dest, src, "batch", "gene", "cell", "counts", relayout = FALSE)
copy_tensor <- function(destination, source,
                        main_axis, rows_axis, columns_axis, name,
                        rows_reaxis = NULL, columns_reaxis = NULL,
                        rename = NULL, type = NULL, empty = NULL,
                        relayout = TRUE, overwrite = FALSE, insist = TRUE) {
    .assert_name(main_axis, "main_axis")
    .assert_name(rows_axis, "rows_axis")
    .assert_name(columns_axis, "columns_axis")
    .assert_name(name, "name")
    if (!is.null(rename)) .assert_name(rename, "rename")
    .assert_flag(relayout, "relayout")
    .assert_flag(overwrite, "overwrite")
    .assert_flag(insist, "insist")
    if (!format_has_axis(destination, main_axis)) {
        stop(sprintf("missing axis: %s in destination", sQuote(main_axis)),
             call. = FALSE)
    }
    base_rename <- if (is.null(rename)) name else rename
    for (entry in format_axis_array(destination, main_axis)) {
        src_mat_name <- paste0(entry, "_", name)
        dest_mat_name <- paste0(entry, "_", base_rename)
        default <- if (is.null(empty)) .DAFR_UNDEF else empty
        copy_matrix(destination, source,
            rows_axis = rows_axis, columns_axis = columns_axis,
            name = src_mat_name,
            rows_reaxis = rows_reaxis, columns_reaxis = columns_reaxis,
            rename = dest_mat_name, type = type,
            default = default, empty = empty,
            relayout = relayout, overwrite = overwrite, insist = insist
        )
    }
    invisible(destination)
}

#' Copy everything from one daf to another.
#'
#' Mirrors Julia `copy_all!(; destination, source, empty, types, overwrite,
#' insist, relayout)`. Copies in order: scalars, axes, vectors, matrices.
#' Tensors are not auto-expanded here — users can call [copy_tensor()]
#' explicitly for per-main-axis-entry matrix groups.
#'
#' Axes already in the destination are not overwritten (regardless of the
#' `overwrite` flag). A destination axis must be identical to or a subset of
#' the source axis (else `empty` is required per-vector / per-matrix to fill
#' missing entries). Unknown-to-source destination axes are left untouched.
#'
#' @param destination A `DafWriter`.
#' @param source A `DafReader`.
#' @param empty Named list mapping flat keys to fill values:
#'   `"axis|name" -> value` for vectors, `"rows|cols|name" -> value` for
#'   matrices. Matrix keys accept either axis order.
#' @param types Named list of type-coercion strings in the same flat-key form
#'   plus `"name"` (no pipes) for scalars.
#' @param overwrite If `TRUE`, replace pre-existing destination entries.
#' @param insist If `TRUE` (default) raise on pre-existing conflicts when
#'   `overwrite = FALSE`; if `FALSE` silently skip.
#' @param relayout If `TRUE` (default), also write transposed layout for
#'   copied matrices.
#' @return Invisibly, the destination.
#' @export
#' @examples
#' src <- memory_daf(name = "src")
#' set_scalar(src, "organism", "human")
#' add_axis(src, "cell", c("c1", "c2"))
#' set_vector(src, "cell", "age", c(10L, 20L))
#' dest <- memory_daf(name = "dest")
#' copy_all(dest, src, relayout = FALSE)
copy_all <- function(destination, source,
                     empty = NULL, types = NULL,
                     overwrite = FALSE, insist = TRUE, relayout = TRUE) {
    .assert_flag(overwrite, "overwrite")
    .assert_flag(insist, "insist")
    .assert_flag(relayout, "relayout")
    # Scalars
    for (nm in format_scalars_set(source)) {
        type <- if (is.null(types)) NULL else types[[nm]]
        copy_scalar(destination, source, nm, type = type,
                    overwrite = overwrite, insist = insist)
    }
    # Axes — only copy axes absent from destination.
    for (ax in format_axes_set(source)) {
        if (!format_has_axis(destination, ax)) {
            copy_axis(destination, source, ax)
        }
    }
    # Vectors — one call per axis x name.
    for (ax in format_axes_set(source)) {
        if (!format_has_axis(destination, ax)) next
        for (vn in format_vectors_set(source, ax)) {
            key <- paste(ax, vn, sep = "|")
            empty_v <- if (is.null(empty)) NULL else empty[[key]]
            type <- if (is.null(types)) NULL else types[[key]]
            copy_vector(destination, source, ax, vn, type = type,
                        empty = empty_v, overwrite = overwrite,
                        insist = insist)
        }
    }
    # Matrices — outer loops over axes.
    axes <- format_axes_set(source)
    for (ra in axes) {
        for (ca in axes) {
            if (!format_has_axis(destination, ra) ||
                !format_has_axis(destination, ca)) next
            for (mn in format_matrices_set(source, ra, ca)) {
                key <- paste(ra, ca, mn, sep = "|")
                alt_key <- paste(ca, ra, mn, sep = "|")
                empty_m <- if (is.null(empty)) NULL else
                           (empty[[key]] %||% empty[[alt_key]])
                type <- if (is.null(types)) NULL else
                        (types[[key]] %||% types[[alt_key]])
                copy_matrix(destination, source, ra, ca, mn, type = type,
                            empty = empty_m, relayout = relayout,
                            overwrite = overwrite, insist = insist)
            }
        }
    }
    invisible(destination)
}

# Null-coalescing operator used by copy_all's empty/types key lookups.
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Build a flat-keyed `empty` (or `types`) list for `copy_all()`.
#'
#' Users can pass a plain named list in the flat-key form directly. This
#' helper assembles one from a more typed builder API. Use `value = ...` for
#' `empty` specs, or `type = ...` for `types` specs.
#'
#' @param vectors List of `list(axis, name, value/type)` records.
#' @param matrices List of `list(rows_axis, columns_axis, name, value/type)`.
#' @param tensors List of `list(main_axis, rows_axis, columns_axis, name,
#'   value/type)`.
#' @param scalars List of `list(name, value/type)` (typically used with
#'   `types`; scalars have no notion of `empty`).
#' @return A named list with flat string keys.
#' @export
#' @examples
#' empty_data(
#'     vectors  = list(list(axis = "cell", name = "age", value = 0L)),
#'     matrices = list(list(rows_axis = "cell", columns_axis = "gene",
#'                          name = "UMIs", value = 0))
#' )
empty_data <- function(vectors = list(), matrices = list(),
                       tensors = list(), scalars = list()) {
    out <- list()
    payload <- function(rec) {
        if (!is.null(rec$value)) rec$value else rec$type
    }
    for (rec in vectors) {
        key <- paste(rec$axis, rec$name, sep = "|")
        out[[key]] <- payload(rec)
    }
    for (rec in matrices) {
        key <- paste(rec$rows_axis, rec$columns_axis, rec$name, sep = "|")
        out[[key]] <- payload(rec)
    }
    for (rec in tensors) {
        key <- paste(rec$main_axis, rec$rows_axis,
                     rec$columns_axis, rec$name, sep = "|")
        out[[key]] <- payload(rec)
    }
    for (rec in scalars) {
        out[[rec$name]] <- payload(rec)
    }
    out
}
