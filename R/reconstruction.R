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
#' The `implicit_axis` property must be a property of strings, where an
#' empty string means "this entry has no value". Data that spells that
#' some other way (`NA`, `Outliers`, a sentinel number) must be passed
#' through [unify_empty_vector_values()] first.
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

    impl_str <- format_get_vector(daf, existing_axis, implicit_axis)$value
    if (!is.character(impl_str)) {
        stop(sprintf(paste0(
            "not a property of strings: %s\nof the axis: %s\nin the daf data: %s\n",
            "use unify_empty_vector_values() to convert it, ",
            "saying which of its values mean nothing"),
            implicit_axis, existing_axis, S7::prop(daf, "name")
        ), call. = FALSE)
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
    empty_values
}

# Julia eltype name -> R storage mode used to build the unified vector.
.UNIFY_STORAGE_MODE <- c(
    String = "character",
    Bool = "logical",
    Float32 = "double", Float64 = "double",
    Int8 = "integer", Int16 = "integer", Int32 = "integer",
    UInt8 = "integer", UInt16 = "integer", UInt32 = "integer",
    Int = "integer64", Int64 = "integer64", UInt64 = "integer64"
)

# The Julia eltype name for what an R vector currently holds. Only used to
# decide whether a requested `dtype` is a conversion or a no-op.
.unify_dtype_of <- function(values) {
    if (is.character(values)) {
        "String"
    } else if (is.logical(values)) {
        "Bool"
    } else if (inherits(values, "integer64")) {
        "Int64"
    } else if (is.integer(values)) {
        "Int32"
    } else {
        "Float64"
    }
}

# Resolve a requested type name to one this can build, accepting the lowercase
# spellings `.canonicalize_julia_type()` already knows plus "string". `[[` on a
# named character vector throws for an unknown name rather than returning NULL,
# so the name has to be checked before it is used as an index.
.unify_resolve_dtype <- function(dtype) {
    known <- names(.UNIFY_STORAGE_MODE)
    if (dtype %in% known) {
        return(dtype)
    }
    matched <- known[tolower(known) == tolower(dtype)]
    if (length(matched) == 1L) {
        return(matched)
    }
    stop(sprintf(
        "unsupported dtype: %s\nmust be one of: %s",
        dtype, paste(known, collapse = ", ")
    ), call. = FALSE)
}

# What "there is no value here" is spelled as, for each kind of type that has
# such a spelling. A signed integer or a Boolean has none.
.unify_default_empty_value <- function(daf, axis, property, dtype) {
    mode <- .UNIFY_STORAGE_MODE[[dtype]]
    if (identical(dtype, "String")) {
        return("")
    }
    if (mode == "double") {
        return(NaN)
    }
    if (startsWith(dtype, "UInt")) {
        return(if (mode == "integer64") bit64::as.integer64(0) else 0L)
    }
    stop(sprintf(
        "no empty value for the type: %s\nof the property: %s\nof the axis: %s\nin the daf data: %s",
        dtype, property, axis, S7::prop(daf, "name")
    ), call. = FALSE)
}

# Convert the values that are not empty, saying which one was not a value of
# the type at all.
.unify_convert_values <- function(daf, axis, property, values, dtype) {
    mode <- .UNIFY_STORAGE_MODE[[dtype]]
    if (mode == "character") {
        return(as.character(values))
    }
    if (is.character(values)) {
        # "true" is a Boolean the way "1.5" is a float; parsing it as a number
        # would call it invalid.
        parsed <- if (mode == "logical") {
            as.logical(values)
        } else {
            suppressWarnings(as.double(values))
        }
        bad <- which(is.na(parsed) & !is.na(values))
        if (length(bad) > 0L) {
            stop(sprintf(
                "invalid value: %s\nfor the type: %s\nof the property: %s\nof the axis: %s\nin the daf data: %s",
                values[[bad[[1L]]]], dtype, property, axis, S7::prop(daf, "name")
            ), call. = FALSE)
        }
        values <- parsed
    }
    if (mode %in% c("integer", "integer64") && !is.null(.INT_TYPE_RANGES[[dtype]])) {
        .check_inexact_int(values, dtype)
    }
    switch(mode,
        logical = as.logical(values),
        integer = as.integer(values),
        integer64 = bit64::as.integer64(values),
        as.double(values)
    )
}

#' Spell "there is no value here" one way in a vector property.
#'
#' Replace every one of the `empty_values` of a `property` of an `axis`
#' with a single `empty_value`, converting the property to a `dtype` on
#' the way if one is given.
#'
#' Data arrives spelling absence several ways, often several ways in the
#' same property: an empty string in some entries and `NA` in others,
#' `(Missing)` elsewhere, and for numbers a sentinel such as the smallest
#' integer, which is a number rather than a visible absence.
#'
#' This matters before [reconstruct_axis()] and [connect_axes()], which
#' decide what to do with an entry by asking whether its value is empty.
#'
#' Numbers often arrive as text for exactly this reason - a column of
#' measurements is a column of strings because a few of its entries say
#' `NA`. Giving a `dtype` converts the values that are not empty, which
#' is an error unless all of them are values of that type; the ones that
#' are empty become the `empty_value`.
#'
#' By default the `empty_value` is the empty string for strings, `NaN`
#' for floats, and `0` for unsigned integers (Daf indices are 1-based, so
#' `0` is free to mean "none"). A signed integer or a Boolean has no such
#' value, so one must be given, or a `dtype` that has one.
#'
#' A property none of whose values is empty is left as it is. What *is*
#' an error is asking for nothing at all - no `empty_values` and no
#' `dtype` - since that cannot do anything whatever the data says.
#'
#' Unlike Julia's `unify_empty_vector_values!`, the result is not
#' `bestify`d; a mostly-empty result is stored as given.
#'
#' @param daf A `DafWriter`.
#' @param axis Axis holding the property.
#' @param property Name of the vector property to rewrite.
#' @param empty_values Value, or vector of values, meaning "there is no
#'   value here". Pass `NULL` (or an empty vector) when only converting.
#' @param dtype Optional Julia type name (`"String"`, `"Float32"`,
#'   `"UInt32"`, ...) to convert the non-empty values to.
#' @param empty_value Optional value to store for the empty entries,
#'   overriding the per-type default.
#' @return `NULL`, invisibly.
#' @export
#' @examples
#' d <- memory_daf(name = "d")
#' add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
#' set_vector(d, "cell", "batch", c("X", "NA", "(Missing)", ""))
#' unify_empty_vector_values(d,
#'     axis = "cell", property = "batch",
#'     empty_values = c("NA", "(Missing)")
#' )
#' get_vector(d, "cell", "batch")
unify_empty_vector_values <- function(daf, axis, property, empty_values,
                                      dtype = NULL, empty_value = NULL) {
    .assert_name(axis, "axis")
    .assert_name(property, "property")
    .require_axis(daf, "for unify_empty_vector_values", axis)
    if (!format_has_vector(daf, axis, property)) {
        stop(sprintf(
            "missing vector: %s\nof the axis: %s\nin the daf data: %s",
            property, axis, S7::prop(daf, "name")
        ), call. = FALSE)
    }

    values <- format_get_vector(daf, axis, property)$value
    current_dtype <- .unify_dtype_of(values)
    dtype <- if (is.null(dtype)) {
        current_dtype
    } else {
        .canonicalize_julia_type(dtype)
    }
    dtype <- .unify_resolve_dtype(dtype)

    empty_values <- unlist(empty_values, use.names = FALSE)
    if (length(empty_values) == 0L && identical(dtype, current_dtype)) {
        stop(sprintf(
            "no empty values and no type to convert to\nof the property: %s\nof the axis: %s\nin the daf data: %s",
            property, axis, S7::prop(daf, "name")
        ), call. = FALSE)
    }

    is_empty <- if (length(empty_values) == 0L) {
        rep(FALSE, length(values))
    } else {
        values %in% empty_values
    }
    mode <- .UNIFY_STORAGE_MODE[[dtype]]
    if (any(is_empty)) {
        if (is.null(empty_value)) {
            empty_value <- .unify_default_empty_value(daf, axis, property, dtype)
        }
        # As the target type, so that filling the empty entries does not widen
        # the result (an integer property staying an integer property).
        empty_value <- switch(mode,
            character = as.character(empty_value),
            logical = as.logical(empty_value),
            integer = as.integer(empty_value),
            integer64 = bit64::as.integer64(empty_value),
            as.double(empty_value)
        )
    }

    unified <- vector(if (mode == "integer64") "double" else mode, length(values))
    if (mode == "integer64") {
        unified <- bit64::as.integer64(unified)
    }
    unified[!is_empty] <-
        .unify_convert_values(daf, axis, property, values[!is_empty], dtype)
    if (any(is_empty)) {
        unified[is_empty] <- empty_value
    }

    format_set_vector(daf, axis, property, unified, overwrite = TRUE)
    invisible(NULL)
}

#' Record which entry of one axis each entry of another belongs to.
#'
#' Given a `base_axis` with two vector properties, one holding a
#' reference to `from_axis` and one to `to_axis`, create a property of
#' `from_axis` that references `to_axis`. This is only possible if every
#' entry of `from_axis` is always associated with a single entry of
#' `to_axis`.
#'
#' This can happen when one axis (say, "batch") references two other axes
#' (say, "plate" and "run"). If every batch had a plate and every plate a
#' run, [reconstruct_axis()] would be enough and batch simply wouldn't
#' have a "run" property. If, however, some batches have a run reference
#' but no plate reference, we still want to record that "each plate is in
#' a run" while not giving up on "each batch is in a run", so the data is
#' duplicated: unlike [reconstruct_axis()], which *moves* data, this
#' *copies* it and leaves the original in place.
#'
#' By default the properties of `base_axis` holding the references are
#' named after the axes they refer to, and the created `connect_property`
#' of `from_axis` is named after `to_axis`. Specify `from_property`,
#' `to_property` and `connect_property` when they are not; a base axis
#' may refer to the same axis twice (a "sorted_by" and a "sequenced_by"
#' run, say), in which case the property name is the only thing telling
#' them apart.
#'
#' An entry of `base_axis` with no `from_axis` reference is skipped, so
#' its `to_axis` reference is not examined at all. An entry of
#' `from_axis` that no entry of `base_axis` refers to is given an empty
#' value.
#'
#' @param daf A `DafWriter`.
#' @param base_axis Axis holding both reference properties.
#' @param from_axis Axis to create the new property on.
#' @param from_property Property of `base_axis` referencing `from_axis`
#'   (default: `from_axis`).
#' @param to_axis Axis the new property references.
#' @param to_property Property of `base_axis` referencing `to_axis`
#'   (default: `to_axis`).
#' @param connect_property Name of the created property of `from_axis`
#'   (default: `to_axis`).
#' @param overwrite Whether to overwrite an existing `connect_property`.
#' @return `NULL`, invisibly.
#' @export
#' @examples
#' d <- memory_daf(name = "d")
#' add_axis(d, "batch", c("B1", "B2", "B3", "B4"))
#' add_axis(d, "plate", c("P1", "P2", "P3"))
#' add_axis(d, "run", c("R1", "R2"))
#' set_vector(d, "batch", "plate", c("P1", "P1", "P2", ""))
#' set_vector(d, "batch", "run", c("R1", "R1", "R2", "R2"))
#' connect_axes(d, base_axis = "batch", from_axis = "plate", to_axis = "run")
#' get_vector(d, "plate", "run") # "R1" "R2" "" -- P3 is named by no batch
connect_axes <- function(daf, base_axis, from_axis, to_axis,
                         from_property = NULL, to_property = NULL,
                         connect_property = NULL, overwrite = FALSE) {
    .assert_name(base_axis, "base_axis")
    .assert_name(from_axis, "from_axis")
    .assert_name(to_axis, "to_axis")
    .assert_flag(overwrite, "overwrite")
    from_property <- from_property %||% from_axis
    to_property <- to_property %||% to_axis
    connect_property <- connect_property %||% to_axis

    daf_name <- S7::prop(daf, "name")
    from_per_base <-
        as.character(format_get_vector(daf, base_axis, from_property)$value)
    to_per_base <-
        as.character(format_get_vector(daf, base_axis, to_property)$value)

    from_names <- format_axis_array(daf, from_axis)$value
    to_names <- format_axis_array(daf, to_axis)$value

    # Each message names the property as well as the axis, since the two need
    # not be named the same, and it is the property that has to be looked at.
    missing_entry <- function(name, axis, property) {
        stop(sprintf(
            "missing entry: %s\nof the axis: %s\nnamed by the property: %s\nof the axis: %s\nin the daf data: %s",
            name, axis, property, base_axis, daf_name
        ), call. = FALSE)
    }

    to_name_of <- stats::setNames(rep(NA_character_, length(from_names)), from_names)
    for (i in seq_along(from_per_base)) {
        from_name <- from_per_base[[i]]
        if (!nzchar(from_name)) {
            next
        }
        if (!(from_name %in% from_names)) {
            missing_entry(from_name, from_axis, from_property)
        }
        to_name <- to_per_base[[i]]
        if (nzchar(to_name) && !(to_name %in% to_names)) {
            missing_entry(to_name, to_axis, to_property)
        }
        previous <- to_name_of[[from_name]]
        if (is.na(previous)) {
            to_name_of[[from_name]] <- to_name
        } else if (!identical(previous, to_name)) {
            # Quoted, unlike the messages above, because here one of the two
            # may be the empty value, and an empty value is a value.
            stop(sprintf(paste0(
                "conflicting entries: \"%s\" != \"%s\"\nof the axis: %s\n",
                "named by the property: %s\nof the axis: %s\nfor the entry: %s\n",
                "of the axis: %s\nnamed by the property: %s\nin the daf data: %s"),
                previous, to_name, to_axis, to_property, base_axis,
                from_name, from_axis, from_property, daf_name
            ), call. = FALSE)
        }
    }

    # Nothing is written until everything above has been verified, so rejected
    # data is left as it was.
    connected <- unname(to_name_of)
    connected[is.na(connected)] <- ""
    format_set_vector(daf, from_axis, connect_property, connected,
                      overwrite = overwrite)
    invisible(NULL)
}
