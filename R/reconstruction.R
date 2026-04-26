#' @include classes.R readers.R writers.R memory_daf.R
NULL

#' Promote an implicit property to an explicit axis.
#'
#' Given an `existing_axis` with a property `implicit_axis`, create a new
#' axis from the unique non-empty values of the property. Scan the other
#' vector properties on `existing_axis`; for each one whose value is
#' uniquely determined by the implicit value, migrate it to the new axis.
#'
#' Returns a named list: for each migrated property, the (consistent) value
#' associated with `existing_axis` entries whose implicit value is empty —
#' or `NULL` if no such entries exist. These values can be used to
#' reconstruct the original property via the `?? X` query modifier.
#'
#' Requires that `rename_axis` (or the default, `implicit_axis` name)
#' does not already exist in `daf`. Merging into a pre-existing axis is
#' not supported.
#'
#' @param daf A `DafWriter`.
#' @param existing_axis Axis that holds the implicit property.
#' @param implicit_axis Property name on `existing_axis`; becomes the new
#'   axis's name (unless `rename_axis`).
#' @param rename_axis Optional name for the new axis.
#' @param empty_implicit If non-NULL, values equal to this are treated as
#'   empty (equivalent to the empty string).
#' @param implicit_properties Optional character vector: only these
#'   properties are considered for migration.
#' @param skipped_properties Optional character vector: properties to
#'   exclude from migration (even if consistent).
#' @return Named list of "value for empty-implicit entries" per migrated
#'   property.
#' @export
#' @examples
#' d <- memory_daf(name = "d")
#' add_axis(d, "cell", c("c1", "c2", "c3"))
#' set_vector(d, "cell", "donor", c("dA", "dB", "dA"))
#' set_vector(d, "cell", "donor_age", c(30L, 40L, 30L))
#' reconstruct_axis(d, existing_axis = "cell", implicit_axis = "donor")
#' get_vector(d, "donor", "donor_age")
reconstruct_axis <- function(daf, existing_axis, implicit_axis,
                             rename_axis = NULL,
                             empty_implicit = NULL,
                             implicit_properties = NULL,
                             skipped_properties = NULL) {
    .assert_name(existing_axis, "existing_axis")
    .assert_name(implicit_axis, "implicit_axis")
    if (!is.null(rename_axis)) .assert_name(rename_axis, "rename_axis")
    new_axis <- if (is.null(rename_axis)) implicit_axis else rename_axis

    if (format_has_axis(daf, new_axis)) {
        stop(sprintf(
            "axis %s already exists; reconstruct_axis does not support merging into a pre-existing axis",
            sQuote(new_axis)
        ), call. = FALSE)
    }
    if (!format_has_vector(daf, existing_axis, implicit_axis)) {
        stop(sprintf(
            "missing vector: %s on axis: %s",
            sQuote(implicit_axis), sQuote(existing_axis)
        ), call. = FALSE)
    }

    impl_vec <- format_get_vector(daf, existing_axis, implicit_axis)
    impl_str <- as.character(impl_vec)
    if (!is.null(empty_implicit)) {
        impl_str[impl_str == as.character(empty_implicit)] <- ""
    }

    non_empty <- impl_str[nzchar(impl_str)]
    unique_vals <- sort(unique(non_empty), method = "radix")
    format_add_axis(daf, new_axis, unique_vals)

    all_vecs <- format_vectors_set(daf, existing_axis)
    all_vecs <- setdiff(all_vecs, implicit_axis)
    if (!is.null(skipped_properties)) {
        all_vecs <- setdiff(all_vecs, skipped_properties)
    }
    if (!is.null(implicit_properties)) {
        all_vecs <- intersect(all_vecs, implicit_properties)
    }

    empty_values <- list()

    for (prop in all_vecs) {
        values <- format_get_vector(daf, existing_axis, prop)
        mapping <- list()
        empty_v <- NULL
        consistent <- TRUE
        for (i in seq_along(impl_str)) {
            k <- impl_str[[i]]
            val <- values[[i]]
            if (!nzchar(k)) {
                if (is.null(empty_v)) {
                    empty_v <- val
                } else if (!identical(empty_v, val)) {
                    consistent <- FALSE; break
                }
                next
            }
            if (is.null(mapping[[k]])) {
                mapping[[k]] <- val
            } else if (!identical(mapping[[k]], val)) {
                consistent <- FALSE; break
            }
        }
        if (!consistent) {
            if (!is.null(implicit_properties)) {
                stop(sprintf(
                    "inconsistent values for the property: %s under the implicit axis: %s",
                    sQuote(prop), sQuote(implicit_axis)
                ), call. = FALSE)
            }
            next
        }
        out <- vapply(unique_vals, function(k) mapping[[k]],
                      FUN.VALUE = values[[1L]])
        format_set_vector(daf, new_axis, prop, out, overwrite = FALSE)
        format_delete_vector(daf, existing_axis, prop, must_exist = TRUE)
        empty_values[[prop]] <- empty_v
    }
    empty_values
}
