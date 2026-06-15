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
#' @param properties_defaults Optional named list: per-property default
#'   value used to fill unused entries of an existing target axis.
#'   When supplied, `reconstruct_axis()` will merge into a
#'   pre-existing axis - the entries listed by the implicit property
#'   must all be present in the axis, and any extras get the
#'   per-property default. Mirrors Julia's
#'   `reconstruct_axis!(..., properties_defaults = (; ...))`.
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
                             skipped_properties = NULL,
                             properties_defaults = NULL) {
    .assert_name(existing_axis, "existing_axis")
    .assert_name(implicit_axis, "implicit_axis")
    if (!is.null(rename_axis)) .assert_name(rename_axis, "rename_axis")
    new_axis <- if (is.null(rename_axis)) implicit_axis else rename_axis

    axis_already_exists <- format_has_axis(daf, new_axis)
    if (axis_already_exists && is.null(properties_defaults)) {
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

    impl_vec <- format_get_vector(daf, existing_axis, implicit_axis)$value
    impl_str <- as.character(impl_vec)
    if (!is.null(empty_implicit)) {
        impl_str[impl_str == as.character(empty_implicit)] <- ""
    }

    non_empty <- impl_str[nzchar(impl_str)]
    if (axis_already_exists) {
        existing_entries <- format_axis_array(daf, new_axis)$value
        unused <- setdiff(unique(non_empty), existing_entries)
        if (length(unused) > 0L) {
            stop(sprintf(
                "implicit values not in existing axis: %s\nfor the axis: %s\nfor the implicit property: %s",
                paste(unused, collapse = ", "), new_axis, implicit_axis
            ), call. = FALSE)
        }
        unique_vals <- existing_entries
    } else {
        unique_vals <- sort(unique(non_empty), method = "radix")
        format_add_axis(daf, new_axis, unique_vals)
    }

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
        values <- format_get_vector(daf, existing_axis, prop)$value
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
        # CR3 parity: when reconstructing into a pre-existing axis,
        # `unique_vals` may include entries not seen via the implicit
        # property. Fill those unused-entry slots from
        # `properties_defaults` if a default for `prop` was supplied;
        # otherwise the property is skipped.
        prop_default <- if (!is.null(properties_defaults))
            properties_defaults[[prop]] else NULL
        unused_keys <- setdiff(unique_vals, names(mapping))
        if (length(unused_keys) > 0L) {
            if (is.null(prop_default)) next
            for (k in unused_keys) mapping[[k]] <- prop_default
        }
        out <- vapply(unique_vals, function(k) mapping[[k]],
                      FUN.VALUE = values[[1L]])
        format_set_vector(daf, new_axis, prop, out, overwrite = FALSE)
        format_delete_vector(daf, existing_axis, prop, must_exist = TRUE)
        # Julia parity: always record a key for the migrated property, even when
        # there were no empty entries (empty_v is NULL). `[[<- NULL` would DELETE
        # the key; `[<- list(NULL)` keeps it with a NULL value.
        empty_values[prop] <- list(empty_v)
    }
    # Julia parity (overwrite_implicit_values): rewrite the implicit property on
    # the existing axis to its string form (a foreign key into the new axis, with
    # "" for empty entries) when it was not already a string, or when a non-empty
    # `empty_implicit` was given. `impl_str` already holds exactly this.
    overwrite_implicit <- !is.character(impl_vec) ||
        (!is.null(empty_implicit) && !identical(as.character(empty_implicit), ""))
    if (overwrite_implicit) {
        format_set_vector(daf, existing_axis, implicit_axis, impl_str,
                          overwrite = TRUE)
    }
    empty_values
}
