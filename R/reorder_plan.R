# Reorder plan structure + walker. Pure functions; no mutation of `daf`.
# Mirrors `DataAxesFormats.jl::Reorder::build_reorder_plan` shape.
#
# Plan shape (named list):
#   $planned_axes     named list (axis name → list(permutation, inverse, new_entries))
#   $planned_vectors  list of list(axis, name, n_replacement_elements)
#   $planned_matrices list of list(rows_axis, columns_axis, name, n_replacement_elements)

# Validate a permutation; compute inverse + new_entries.
.reorder_planned_axis <- function(permutation, entries) {
    perm <- as.integer(permutation)
    n <- length(entries)
    if (length(perm) != n) {
        stop(sprintf(
            "permutation length %d does not match axis length %d",
            length(perm), n
        ), call. = FALSE)
    }
    if (anyNA(perm) || any(perm < 1L) || any(perm > n) ||
        anyDuplicated(perm)) {
        stop("permutation must be a permutation of 1:n", call. = FALSE)
    }
    inverse <- integer(n)
    inverse[perm] <- seq_len(n)
    list(
        permutation = perm,
        inverse = inverse,
        new_entries = entries[perm]
    )
}

# Walk `daf` and produce a reorder plan for the given `permutations`
# (named list mapping axis names to integer permutations). Axes that
# don't exist on `daf` are silently skipped.
.build_reorder_plan <- function(daf, permutations) {
    if (!is.list(permutations) || is.null(names(permutations))) {
        stop("permutations must be a named list", call. = FALSE)
    }

    planned_axes <- list()
    for (axis in names(permutations)) {
        if (!format_has_axis(daf, axis)) next
        entries <- format_axis_array(daf, axis)$value
        planned_axes[[axis]] <- .reorder_planned_axis(
            permutations[[axis]], entries
        )
    }

    planned_vectors <- list()
    for (axis in names(planned_axes)) {
        for (name in format_vectors_set(daf, axis)) {
            v <- format_get_vector(daf, axis, name)$value
            planned_vectors[[length(planned_vectors) + 1L]] <- list(
                axis = axis,
                name = name,
                n_replacement_elements = length(v)
            )
        }
    }

    planned_matrices <- list()
    for (perm_axis in names(planned_axes)) {
        for (other_axis in format_axes_set(daf)) {
            for (name in format_matrices_set(daf, perm_axis, other_axis)) {
                m <- format_get_matrix(daf, perm_axis, other_axis, name)$value
                n_elem <- if (methods::is(m, "sparseMatrix")) {
                    length(m@x)
                } else {
                    length(m)
                }
                planned_matrices[[length(planned_matrices) + 1L]] <- list(
                    rows_axis = perm_axis,
                    columns_axis = other_axis,
                    name = name,
                    n_replacement_elements = n_elem
                )
            }
            # Also catch matrices on (other_axis, perm_axis), but skip
            # if other_axis is also being permuted (already handled by
            # the (perm_axis, other_axis) iteration when other_axis is
            # the perm_axis of an outer iteration).
            if (!(other_axis %in% names(planned_axes))) {
                for (name in format_matrices_set(daf, other_axis, perm_axis)) {
                    m <- format_get_matrix(daf, other_axis, perm_axis, name)$value
                    n_elem <- if (methods::is(m, "sparseMatrix")) {
                        length(m@x)
                    } else {
                        length(m)
                    }
                    planned_matrices[[length(planned_matrices) + 1L]] <- list(
                        rows_axis = other_axis,
                        columns_axis = perm_axis,
                        name = name,
                        n_replacement_elements = n_elem
                    )
                }
            }
        }
    }

    list(
        planned_axes = planned_axes,
        planned_vectors = planned_vectors,
        planned_matrices = planned_matrices
    )
}
