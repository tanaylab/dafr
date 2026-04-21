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
