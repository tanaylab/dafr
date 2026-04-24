#' @include classes.R readers.R writers.R memory_daf.R copies.R
NULL

#' Merge action constants for [concatenate()].
#'
#' Used as values in `concatenate()`'s `merge` argument:
#' `MERGE_SKIP` skips the property (default), `MERGE_LAST_VALUE` uses the
#' last source containing it, `MERGE_COLLECT_AXIS` collects values along
#' the `dataset_axis`.
#'
#' @name merge_actions
#' @export
MERGE_SKIP <- "SkipProperty"
#' @rdname merge_actions
#' @export
MERGE_LAST_VALUE <- "LastValue"
#' @rdname merge_actions
#' @export
MERGE_COLLECT_AXIS <- "CollectAxis"

#' Concatenate multiple dafs along one or more axes.
#'
#' Mirrors Julia `concatenate!()`. For each concatenation axis, entries from
#' each source are appended in source order. Non-concat axes must be
#' identical across all sources and are copied once.
#'
#' @param destination A `DafWriter`. Must be empty of the concatenation axes.
#' @param axis A single axis name or a character vector of axis names.
#' @param sources List of `DafReader`s to concatenate.
#' @param names Optional character vector of unique data set names (defaults
#'   to each source's `name` prop).
#' @param dataset_axis Name of the per-source axis to create. `NULL` disables.
#' @param dataset_property If `TRUE` (default) and `dataset_axis` is non-NULL,
#'   create a same-named vector on every concatenation axis holding the
#'   source name for each entry.
#' @param prefix Logical (single or per-axis). Prefix concat-axis entries
#'   with `"<dataset_name>."` to de-duplicate across sources.
#' @param prefixed Optional character vector (or list of vectors per axis)
#'   of additional property names to prefix, beyond the heuristic (same-name
#'   or `"<axis>.*"` properties).
#' @param empty Named list of fill values for missing per-source properties.
#' @param merge Named list mapping property keys to a merge action
#'   (`"SkipProperty"`, `"LastValue"`, `"CollectAxis"`).
#' @param sparse_if_saves_storage_fraction Numeric (default 0.25); reserved
#'   for a future sparse-promotion heuristic.
#' @param overwrite If `TRUE`, allow replacing pre-existing destination
#'   entries.
#' @return Invisibly, the destination.
#' @export
#' @examples
#' a <- memory_daf(name = "A"); add_axis(a, "cell", c("a1", "a2"))
#' b <- memory_daf(name = "B"); add_axis(b, "cell", c("b1"))
#' dest <- memory_daf(name = "dest")
#' concatenate(dest, "cell", list(a, b))
concatenate <- function(destination, axis, sources,
                        names = NULL,
                        dataset_axis = "dataset",
                        dataset_property = TRUE,
                        prefix = FALSE,
                        prefixed = NULL,
                        empty = NULL,
                        merge = NULL,
                        sparse_if_saves_storage_fraction = 0.25,
                        overwrite = FALSE) {
    axes <- if (is.character(axis) && length(axis) > 1L) axis else as.character(axis)
    if (length(axes) < 1L) stop("`axis` must be a non-empty character vector",
                                call. = FALSE)
    if (anyDuplicated(axes)) stop("`axis` names must be unique", call. = FALSE)
    for (ax in axes) .assert_name(ax, "axis")
    .assert_flag(overwrite, "overwrite")

    if (length(sources) < 1L) stop("`sources` must be non-empty",
                                   call. = FALSE)
    dataset_names <- .concat_dataset_names(sources, names)

    # Validate: no matrix has both axes in the concat set.
    for (src in sources) {
        for (ra in axes) for (ca in axes) {
            if (format_has_axis(src, ra) && format_has_axis(src, ca) &&
                length(format_matrices_set(src, ra, ca)) > 0L) {
                stop(sprintf(
                    "can't concatenate a matrix with both axes in the concat set: %s,%s in %s",
                    sQuote(ra), sQuote(ca), S7::prop(src, "name")
                ), call. = FALSE)
            }
        }
    }

    # Per-axis prefix flag.
    prefixes <- if (is.logical(prefix) && length(prefix) == 1L) {
        rep(prefix, length(axes))
    } else if (is.logical(prefix) && length(prefix) == length(axes)) {
        prefix
    } else {
        stop("`prefix` must be a logical scalar or a logical vector of length(axis)",
             call. = FALSE)
    }
    base::names(prefixes) <- axes

    # Create non-concat axes from the first source; verify all sources agree.
    for (src in sources) {
        for (ax in format_axes_set(src)) {
            if (ax %in% axes) next
            if (!format_has_axis(destination, ax)) {
                format_add_axis(destination, ax, format_axis_array(src, ax))
            } else if (!identical(format_axis_array(destination, ax),
                                  format_axis_array(src, ax))) {
                stop(sprintf(
                    "different entries for the axis: %s between dafs", sQuote(ax)
                ), call. = FALSE)
            }
        }
    }

    # Create the concatenation axes.
    for (ax in axes) {
        .concat_one_axis(destination, ax, sources, dataset_names,
                         prefixes[[ax]], prefixes, prefixed, empty, overwrite,
                         sparse_if_saves_storage_fraction)
    }

    # Optional dataset_axis + property.
    if (!is.null(dataset_axis)) {
        format_add_axis(destination, dataset_axis, dataset_names)
        if (isTRUE(dataset_property)) {
            for (ax in axes) {
                offsets <- cumsum(c(0L, vapply(sources,
                    function(s) format_axis_length(s, ax), integer(1L))))
                labels <- character(offsets[[length(offsets)]])
                for (i in seq_along(sources)) {
                    labels[(offsets[[i]] + 1L):offsets[[i + 1L]]] <- dataset_names[[i]]
                }
                format_set_vector(destination, ax, dataset_axis, labels,
                                  overwrite = overwrite)
            }
        }
    }

    # Merge pass for properties not on any concat axis.
    if (!is.null(merge)) {
        .concat_merge(destination, sources, dataset_names, dataset_axis,
                      axes, merge, overwrite)
    }

    invisible(destination)
}

.concat_dataset_names <- function(sources, names) {
    if (is.null(names)) {
        names <- vapply(sources, function(s) S7::prop(s, "name"),
                        character(1L))
    } else {
        if (length(names) != length(sources)) {
            stop("`names` must have one entry per source", call. = FALSE)
        }
    }
    if (anyDuplicated(names)) {
        stop("dataset `names` must be unique", call. = FALSE)
    }
    names
}

.concat_one_axis <- function(destination, axis, sources, dataset_names,
                             do_prefix, all_prefixes, prefixed, empty,
                             overwrite, sparse_threshold) {
    per_src <- lapply(seq_along(sources), function(i) {
        e <- format_axis_array(sources[[i]], axis)
        if (isTRUE(do_prefix)) paste(dataset_names[[i]], e, sep = ".") else e
    })
    combined <- unlist(per_src, use.names = FALSE)
    if (anyDuplicated(combined)) {
        stop(sprintf("duplicate entries on axis %s across sources; use prefix = TRUE",
                     sQuote(axis)), call. = FALSE)
    }
    format_add_axis(destination, axis, combined)

    # Union of vector names across sources for this axis.
    all_vec_names <- unique(unlist(lapply(sources,
        function(s) format_vectors_set(s, axis)), use.names = FALSE))

    for (vn in all_vec_names) {
        .concat_axis_vector(destination, axis, vn, sources, dataset_names,
                            do_prefix, all_prefixes, prefixed, empty,
                            overwrite, sparse_threshold)
    }

    # Matrices with `axis` as one side and some other-axis on the other.
    other_axes <- unique(unlist(lapply(sources, format_axes_set)))
    other_axes <- setdiff(other_axes, axis)
    for (oa in other_axes) {
        all_mat_names <- unique(unlist(lapply(sources, function(s) {
            if (format_has_axis(s, axis) && format_has_axis(s, oa)) {
                c(format_matrices_set(s, axis, oa),
                  format_matrices_set(s, oa, axis))
            } else character(0L)
        })))
        for (mn in all_mat_names) {
            .concat_axis_matrix(destination, axis, oa, mn, sources,
                                empty, overwrite, sparse_threshold)
        }
    }
}

.concat_axis_vector <- function(destination, axis, name, sources,
                                dataset_names, do_prefix, all_prefixes,
                                prefixed, empty, overwrite, sparse_threshold) {
    parts <- vector("list", length(sources))
    for (i in seq_along(sources)) {
        src <- sources[[i]]
        if (format_has_vector(src, axis, name)) {
            v <- format_get_vector(src, axis, name)
        } else {
            key <- paste(axis, name, sep = "|")
            fill <- if (is.null(empty)) NULL else empty[[key]]
            if (is.null(fill)) {
                stop(sprintf(
                    "no empty value for the vector: %s of the axis: %s which is missing from the daf data: %s",
                    sQuote(name), sQuote(axis), S7::prop(src, "name")
                ), call. = FALSE)
            }
            v <- rep(fill, format_axis_length(src, axis))
        }
        is_prefix_target <- {
            if (!is.null(prefixed)) {
                vec <- if (is.list(prefixed)) prefixed[[axis]] else prefixed
                isTRUE(do_prefix) && name %in% vec
            } else {
                # Heuristic: prefix if the property name matches (or looks
                # like a subproperty of) any concat axis that is itself
                # prefixed. This remaps cross-axis references so that
                # `cell|cluster` values still line up with the (prefixed)
                # cluster axis entries.
                prefixed_axes <- base::names(all_prefixes)[
                    vapply(all_prefixes, isTRUE, logical(1L))]
                any(vapply(prefixed_axes, function(ax) {
                    name == ax || startsWith(name, paste0(ax, "."))
                }, logical(1L)))
            }
        }
        if (is_prefix_target && is.character(v)) {
            v <- paste(dataset_names[[i]], v, sep = ".")
        }
        parts[[i]] <- v
    }
    out <- do.call(c, parts)
    format_set_vector(destination, axis, name, out, overwrite = overwrite)
}

.concat_axis_matrix <- function(destination, axis, other_axis, name, sources,
                                empty, overwrite, sparse_threshold) {
    parts <- vector("list", length(sources))
    for (i in seq_along(sources)) {
        src <- sources[[i]]
        have_ao <- format_has_axis(src, axis) && format_has_axis(src, other_axis)
        if (!have_ao) {
            stop(sprintf("source %s missing axes for matrix %s",
                         S7::prop(src, "name"), sQuote(name)), call. = FALSE)
        }
        mat <- if (format_has_matrix(src, other_axis, axis, name)) {
            format_get_matrix(src, other_axis, axis, name)
        } else if (format_has_matrix(src, axis, other_axis, name)) {
            m <- format_get_matrix(src, axis, other_axis, name)
            if (inherits(m, "dgCMatrix")) Matrix::t(m) else t(m)
        } else {
            key <- paste(other_axis, axis, name, sep = "|")
            alt_key <- paste(axis, other_axis, name, sep = "|")
            fill <- if (is.null(empty)) NULL else (empty[[key]] %||% empty[[alt_key]])
            if (is.null(fill)) {
                stop(sprintf(
                    "no empty value for the matrix: %s of the rows axis: %s and the columns axis: %s which is missing from the daf data: %s",
                    sQuote(name), sQuote(other_axis), sQuote(axis),
                    S7::prop(src, "name")
                ), call. = FALSE)
            }
            matrix(fill,
                   nrow = format_axis_length(src, other_axis),
                   ncol = format_axis_length(src, axis))
        }
        parts[[i]] <- mat
    }
    if (all(vapply(parts, inherits, logical(1L), "dgCMatrix"))) {
        combined <- do.call(cbind, parts)
    } else {
        combined <- do.call(cbind, lapply(parts, function(m)
            if (inherits(m, "dgCMatrix")) as.matrix(m) else m))
    }
    dest_rows <- format_axis_array(destination, other_axis)
    dest_cols <- format_axis_array(destination, axis)
    dimnames(combined) <- list(dest_rows, dest_cols)
    format_set_matrix(destination, other_axis, axis, name, combined,
                      overwrite = overwrite)
}

.concat_merge <- function(destination, sources, dataset_names, dataset_axis,
                          concat_axes, merge, overwrite) {
    for (prop_key in base::names(merge)) {
        action <- merge[[prop_key]]
        parts <- strsplit(prop_key, "|", fixed = TRUE)[[1L]]
        if (length(parts) == 1L) {
            .concat_merge_scalar(destination, sources, dataset_names,
                                 dataset_axis, parts[[1L]], action, overwrite)
        } else if (length(parts) == 2L) {
            if (parts[[1L]] %in% concat_axes) next
            .concat_merge_vector(destination, sources, dataset_names,
                                 dataset_axis, parts[[1L]], parts[[2L]],
                                 action, overwrite)
        } else if (length(parts) == 3L) {
            if (action == MERGE_COLLECT_AXIS) {
                stop(sprintf(
                    "can't CollectAxis for a matrix: %s (would create a 3D tensor)",
                    sQuote(prop_key)
                ), call. = FALSE)
            }
        }
    }
}

.concat_merge_scalar <- function(destination, sources, dataset_names,
                                 dataset_axis, name, action, overwrite) {
    if (action == MERGE_SKIP) return(invisible())
    if (action == MERGE_LAST_VALUE) {
        for (i in rev(seq_along(sources))) {
            if (format_has_scalar(sources[[i]], name)) {
                format_set_scalar(destination, name,
                                  format_get_scalar(sources[[i]], name),
                                  overwrite = overwrite)
                return(invisible())
            }
        }
        return(invisible())
    }
    if (action == MERGE_COLLECT_AXIS) {
        if (is.null(dataset_axis)) {
            stop(sprintf(
                "can't collect axis for the scalar: %s because no dataset axis was created",
                sQuote(name)
            ), call. = FALSE)
        }
        vals <- lapply(sources, function(s)
            if (format_has_scalar(s, name)) format_get_scalar(s, name) else NA)
        format_set_vector(destination, dataset_axis, name,
                          do.call(c, vals), overwrite = overwrite)
    }
}

.concat_merge_vector <- function(destination, sources, dataset_names,
                                 dataset_axis, axis, name, action, overwrite) {
    if (action == MERGE_SKIP) return(invisible())
    if (action == MERGE_LAST_VALUE) {
        for (i in rev(seq_along(sources))) {
            if (format_has_vector(sources[[i]], axis, name)) {
                format_set_vector(destination, axis, name,
                                  format_get_vector(sources[[i]], axis, name),
                                  overwrite = overwrite)
                return(invisible())
            }
        }
        return(invisible())
    }
    if (action == MERGE_COLLECT_AXIS) {
        if (is.null(dataset_axis)) {
            stop(sprintf(
                "can't collect axis for the vector: %s on axis %s because no dataset axis was created",
                sQuote(name), sQuote(axis)
            ), call. = FALSE)
        }
        src_len <- format_axis_length(destination, axis)
        n_src <- length(sources)
        out <- matrix(NA, nrow = src_len, ncol = n_src,
                      dimnames = list(format_axis_array(destination, axis),
                                      dataset_names))
        for (i in seq_along(sources)) {
            s <- sources[[i]]
            if (format_has_vector(s, axis, name)) {
                out[, i] <- format_get_vector(s, axis, name)
            }
        }
        format_set_matrix(destination, axis, dataset_axis, name, out,
                          overwrite = overwrite)
    }
}
