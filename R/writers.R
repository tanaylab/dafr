#' Add a new axis.
#'
#' @param daf A `DafWriter`.
#' @param axis Axis name.
#' @param entries Unique, non-NA, non-empty character vector of entry names.
#' @return Invisibly the input `daf`.
#' @examples
#' # Mirrors writers.jl jldoctest at line 161.
#' d <- example_cells_daf()
#' has_axis(d, "block")                  # FALSE
#' add_axis(d, "block", c("B1", "B2"))
#' has_axis(d, "block")                  # TRUE
#' @export
add_axis <- function(daf, axis, entries) {
    .assert_name(axis, "axis")
    stopifnot(is.character(entries))
    .cli_verbose(
        "add_axis %s (%d entries) on %s",
        axis, length(entries), S7::prop(daf, "name")
    )
    format_add_axis(daf, axis, entries)
    invisible(daf)
}

#' Delete an axis (and all vectors / matrices that depend on it).
#'
#' @inheritParams add_axis
#' @param must_exist If `TRUE` (default) raise when the axis is absent;
#'   if `FALSE` silently no-op.
#' @return Invisibly the input `daf`.
#' @examples
#' # Mirrors writers.jl jldoctest at line 217.
#' m <- example_metacells_daf()
#' has_axis(m, "type")    # TRUE
#' delete_axis(m, "type")
#' has_axis(m, "type")    # FALSE
#' @export
delete_axis <- function(daf, axis, must_exist = TRUE) {
    .assert_name(axis, "axis")
    .assert_flag(must_exist, "must_exist")
    format_delete_axis(daf, axis, must_exist)
    invisible(daf)
}

#' Set a scalar.
#' @inheritParams has_scalar
#' @param value Atomic scalar (length 1, non-NA).
#' @param overwrite If `FALSE` (default) error when the scalar already
#'   exists; if `TRUE` replace.
#' @return Invisibly the input `daf`.
#' @examples
#' # Mirrors writers.jl jldoctest at line 63.
#' d <- example_cells_daf()
#' set_scalar(d, "version", 1.0)
#' get_scalar(d, "version")                          # 1.0
#' set_scalar(d, "version", 2.0, overwrite = TRUE)
#' get_scalar(d, "version")                          # 2.0
#' @export
set_scalar <- function(daf, name, value, overwrite = FALSE) {
    .assert_name(name, "name")
    .assert_flag(overwrite, "overwrite")
    format_set_scalar(daf, name, value, overwrite)
    invisible(daf)
}

#' Delete a scalar.
#' @inheritParams has_scalar
#' @param must_exist See `delete_axis`.
#' @return Invisibly the input `daf`.
#' @examples
#' # Mirrors writers.jl jldoctest at line 109.
#' d <- example_cells_daf()
#' has_scalar(d, "organism")    # TRUE
#' delete_scalar(d, "organism")
#' has_scalar(d, "organism")    # FALSE
#' @export
delete_scalar <- function(daf, name, must_exist = TRUE) {
    .assert_name(name, "name")
    .assert_flag(must_exist, "must_exist")
    format_delete_scalar(daf, name, must_exist)
    invisible(daf)
}

#' Set a vector on an axis.
#'
#' @inheritParams has_vector
#' @param vec Atomic vector of length `axis_length(daf, axis)`, or a
#'   named vector whose names are a subset of the axis entries (reordered
#'   into axis order at storage time).
#' @param overwrite See `set_scalar`.
#' @return Invisibly the input `daf`.
#' @examples
#' # Mirrors writers.jl jldoctest at line 300.
#' m <- example_metacells_daf()
#' has_vector(m, "type", "is_mebemp")                       # FALSE
#' set_vector(m, "type", "is_mebemp", c(TRUE, TRUE, FALSE, FALSE))
#' has_vector(m, "type", "is_mebemp")                       # TRUE
#' set_vector(m, "type", "is_mebemp",
#'            c(TRUE, TRUE, TRUE, FALSE), overwrite = TRUE)
#' has_vector(m, "type", "is_mebemp")                       # TRUE
#' @export
set_vector <- function(daf, axis, name, vec, overwrite = FALSE) {
    .assert_name(axis, "axis")
    .assert_name(name, "name")
    .assert_flag(overwrite, "overwrite")
    .require_not_reserved(daf, axis, name)
    format_set_vector(daf, axis, name, vec, overwrite)
    invisible(daf)
}

#' Delete a vector on an axis.
#'
#' @inheritParams has_vector
#' @param must_exist See `delete_axis`.
#' @return Invisibly the input `daf`.
#' @examples
#' # Mirrors writers.jl jldoctest at line 618.
#' m <- example_metacells_daf()
#' has_vector(m, "type", "color")    # TRUE
#' delete_vector(m, "type", "color")
#' has_vector(m, "type", "color")    # FALSE
#' @export
delete_vector <- function(daf, axis, name, must_exist = TRUE) {
    .assert_name(axis, "axis")
    .assert_name(name, "name")
    .assert_flag(must_exist, "must_exist")
    .require_not_reserved(daf, axis, name)
    format_delete_vector(daf, axis, name, must_exist)
    invisible(daf)
}

#' Set a matrix indexed by a pair of axes.
#'
#' @inheritParams has_matrix
#' @param mat Dense `matrix`, or sparse `dgCMatrix` / `lgCMatrix`, of
#'   shape `axis_length(rows_axis) x axis_length(columns_axis)`.
#' @param overwrite See `set_scalar`.
#' @param relayout If `TRUE`, also store the flipped layout (so
#'   `get_matrix(columns_axis, rows_axis, name)` skips the
#'   transpose-on-the-fly path). Mirrors Julia `set_matrix!(...; relayout)`.
#'   Default `FALSE`; set to `TRUE` to match Julia's default behavior.
#' @return Invisibly the input `daf`.
#' @examples
#' # Mirrors writers.jl jldoctest at line 686.
#' m <- example_metacells_daf()
#' has_matrix(m, "gene", "metacell", "confidence", relayout = FALSE)    # FALSE
#' set_matrix(m, "metacell", "gene", "confidence",
#'            matrix(stats::runif(7L * 683L), 7L, 683L), relayout = FALSE)
#' has_matrix(m, "gene", "metacell", "confidence", relayout = FALSE)    # FALSE
#' has_matrix(m, "metacell", "gene", "confidence", relayout = FALSE)    # TRUE
#' set_matrix(m, "metacell", "gene", "confidence",
#'            matrix(stats::runif(7L * 683L), 7L, 683L),
#'            overwrite = TRUE, relayout = TRUE)
#' has_matrix(m, "gene", "metacell", "confidence", relayout = FALSE)    # TRUE
#' has_matrix(m, "metacell", "gene", "confidence", relayout = FALSE)    # TRUE
#' @export
set_matrix <- function(daf, rows_axis, columns_axis, name, mat,
                       overwrite = FALSE, relayout = FALSE) {
    .assert_name(rows_axis, "rows_axis")
    .assert_name(columns_axis, "columns_axis")
    .assert_name(name, "name")
    .assert_flag(overwrite, "overwrite")
    .assert_flag(relayout, "relayout")
    format_set_matrix(daf, rows_axis, columns_axis, name, mat, overwrite)
    if (relayout && rows_axis != columns_axis) {
        format_relayout_matrix(daf, rows_axis, columns_axis, name)
    }
    invisible(daf)
}

#' Delete a matrix.
#' @inheritParams has_matrix
#' @param must_exist See `delete_axis`.
#' @param relayout If `TRUE` (default), also delete the flipped layout
#'   `(columns_axis, rows_axis, name)` if present. Mirrors Julia
#'   `delete_matrix!(...; relayout)`.
#' @return Invisibly the input `daf`.
#' @examples
#' # Mirrors writers.jl jldoctest at line 1147.
#' d <- example_cells_daf()
#' has_matrix(d, "gene", "cell", "UMIs", relayout = FALSE)  # TRUE
#' has_matrix(d, "cell", "gene", "UMIs", relayout = FALSE)  # TRUE
#' delete_matrix(d, "gene", "cell", "UMIs", relayout = FALSE)
#' has_matrix(d, "gene", "cell", "UMIs", relayout = FALSE)  # FALSE
#' has_matrix(d, "cell", "gene", "UMIs", relayout = FALSE)  # TRUE
#' delete_matrix(d, "gene", "cell", "UMIs", must_exist = FALSE)
#' has_matrix(d, "gene", "cell", "UMIs", relayout = FALSE)  # FALSE
#' has_matrix(d, "cell", "gene", "UMIs", relayout = FALSE)  # FALSE
#' @export
delete_matrix <- function(daf, rows_axis, columns_axis, name,
                          must_exist = TRUE, relayout = TRUE) {
    .assert_name(rows_axis, "rows_axis")
    .assert_name(columns_axis, "columns_axis")
    .assert_name(name, "name")
    .assert_flag(must_exist, "must_exist")
    .assert_flag(relayout, "relayout")
    relayout <- relayout && rows_axis != columns_axis
    if (must_exist && !format_has_matrix(daf, rows_axis, columns_axis, name) &&
        !(relayout && format_has_matrix(daf, columns_axis, rows_axis, name))) {
        .require_matrix(daf, rows_axis, columns_axis, name, relayout = relayout)
    }
    if (format_has_matrix(daf, rows_axis, columns_axis, name)) {
        format_delete_matrix(daf, rows_axis, columns_axis, name, must_exist = FALSE)
    }
    if (relayout && format_has_matrix(daf, columns_axis, rows_axis, name)) {
        format_delete_matrix(daf, columns_axis, rows_axis, name, must_exist = FALSE)
    }
    invisible(daf)
}

#' Physically store the transposed layout of a matrix.
#'
#' After this call, `get_matrix(columns_axis, rows_axis, name)` skips the
#' transpose-on-the-fly path.
#'
#' @inheritParams has_matrix
#' @param overwrite If `TRUE`, replace any existing matrix at the
#'   transposed `(columns_axis, rows_axis, name)` location. If `FALSE`
#'   (default), raise when such a matrix already exists.
#' @return Invisibly the input `daf`.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "cell", c("c1", "c2"))
#' add_axis(d, "gene", c("g1", "g2", "g3"))
#' m <- matrix(1:6, nrow = 2, ncol = 3)
#' set_matrix(d, "cell", "gene", "counts", m)
#' relayout_matrix(d, "cell", "gene", "counts")
#' has_matrix(d, "gene", "cell", "counts")
#' @export
relayout_matrix <- function(daf, rows_axis, columns_axis, name, overwrite = FALSE) {
    .assert_name(rows_axis, "rows_axis")
    .assert_name(columns_axis, "columns_axis")
    .assert_name(name, "name")
    .assert_flag(overwrite, "overwrite")
    .require_axis(daf, sprintf("for the rows of the matrix: %s", name), rows_axis)
    .require_axis(daf, sprintf("for the columns of the matrix: %s", name), columns_axis)
    if (rows_axis == columns_axis) {
        stop(sprintf(
            "can't relayout square matrix: %s\nof the axis: %s\ndue to daf representation limitations\nin the daf data: %s",
            name, rows_axis, S7::prop(daf, "name")
        ), call. = FALSE)
    }
    .require_matrix(daf, rows_axis, columns_axis, name, relayout = FALSE)
    if (!overwrite) {
        .require_no_matrix(daf, columns_axis, rows_axis, name, relayout = FALSE)
    }
    format_relayout_matrix(daf, rows_axis, columns_axis, name)
    invisible(daf)
}

# Centralized "must not exist" helpers; emit the EXACT message text used by
# DataAxesFormats.jl writers.jl (`require_no_*`). One-line forms for scalar /
# axis, multi-line forms for vector / matrix, all rendering `daf.name` from
# the S7 `name` slot.

.require_no_scalar <- function(daf, name) {
    if (format_has_scalar(daf, name)) {
        stop(sprintf("existing scalar: %s\nin the daf data: %s",
                     name, S7::prop(daf, "name")),
             call. = FALSE)
    }
    invisible(NULL)
}

.require_no_axis <- function(daf, axis) {
    if (format_has_axis(daf, axis)) {
        stop(sprintf("existing axis: %s\nin the daf data: %s",
                     axis, S7::prop(daf, "name")),
             call. = FALSE)
    }
    invisible(NULL)
}

.require_no_vector <- function(daf, axis, name) {
    if (format_has_vector(daf, axis, name)) {
        stop(sprintf("existing vector: %s\nfor the axis: %s\nin the daf data: %s",
                     name, axis, S7::prop(daf, "name")),
             call. = FALSE)
    }
    invisible(NULL)
}

.require_not_reserved <- function(daf, axis, name) {
    if (name == "name" || name == "index") {
        stop(sprintf(
            "setting the reserved vector: %s\nfor the axis: %s\nin the daf data: %s",
            name, axis, S7::prop(daf, "name")
        ), call. = FALSE)
    }
    invisible(NULL)
}

.require_no_matrix <- function(daf, rows_axis, columns_axis, name, relayout = TRUE) {
    if (format_has_matrix(daf, rows_axis, columns_axis, name)) {
        stop(sprintf(
            "existing matrix: %s\nfor the rows axis: %s\nand the columns axis: %s\nin the daf data: %s",
            name, rows_axis, columns_axis, S7::prop(daf, "name")
        ), call. = FALSE)
    }
    if (isTRUE(relayout)) {
        .require_no_matrix(daf, columns_axis, rows_axis, name, relayout = FALSE)
    }
    invisible(NULL)
}
