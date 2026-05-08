#' Reorder one or more axis entries in place.
#'
#' Permutes the entries of one or more axes in the given daf, rewriting
#' every vector and matrix that depends on those axes. On `files_daf`
#' the operation is crash-recoverable via a `.reorder.backup/`
#' directory of hardlinks; on the next open with mode `"r+"` or `"w+"`,
#' any in-progress reorder is automatically rolled back to the
#' pre-reorder state.
#'
#' Each `...` argument is a name = integer-permutation pair, where the
#' permutation is a 1-based vector of length `axis_length(daf, name)`.
#' After reorder, the entry at position `i` is the entry that was at
#' position `permutation[i]` before — i.e. `new_entries[i] =
#' old_entries[permutation[i]]`.
#'
#' @param daf A [DafWriter].
#' @param ... Named permutations: `cell = perm_cell, gene = perm_gene`.
#' @param crash_counter Internal — for testing only. See
#'   `tick_crash_counter()` (not exported).
#' @return Invisibly the input `daf`.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "cell", c("A", "B", "C"))
#' set_vector(d, "cell", "x", c(10, 20, 30))
#' reorder_axes(d, cell = c(3L, 1L, 2L))
#' axis_vector(d, "cell")
#' get_vector(d, "cell", "x")
#' @export
reorder_axes <- function(daf, ..., crash_counter = NULL) {
    permutations <- list(...)
    if (length(permutations) == 0L) {
        return(invisible(daf))
    }
    if (is.null(names(permutations)) ||
        any(!nzchar(names(permutations)))) {
        stop("reorder_axes: all `...` arguments must be named", call. = FALSE)
    }

    # Multi-writer dispatch: a bare list of DafWriters reorders all of
    # them in one call (Julia: reorder_axes!([d1, d2], Dict(...))).
    is_multi <- is.list(daf) && !S7::S7_inherits(daf, DafReader)
    if (is_multi) {
        return(.reorder_axes_multi(daf, permutations, crash_counter))
    }

    if (!is_leaf(daf)) {
        stop(sprintf(
            "non-leaf type: %s\nfor the daf data: %s\ngiven to reorder_axes",
            .daf_type_name(daf), S7::prop(daf, "name")
        ), call. = FALSE)
    }
    plan <- .build_reorder_plan(daf, permutations)
    format_replace_reorder(daf, plan, crash_counter = crash_counter)
    format_cleanup_reorder(daf, plan, crash_counter = crash_counter)
    invisible(daf)
}

# Multi-writer orchestrator. Mirrors DAF.jl reorder_axes!(::Vector, Dict)
# at src/reorder.jl. Each axis must agree on entry order across all
# writers that have it; writers missing the axis silently skip it.
# Sort key prefers complete_path() for deterministic ordering, falling
# back to a stable string key for memory-only writers.
.reorder_axes_multi <- function(dafs, permutations, crash_counter) {
    if (length(dafs) == 0L) return(invisible(dafs))
    for (d in dafs) {
        if (!S7::S7_inherits(d, DafWriter)) {
            stop("reorder_axes: list elements must be DafWriter instances",
                 call. = FALSE)
        }
        if (!is_leaf(d)) {
            stop(sprintf(
                "non-leaf type: %s\nfor the daf data: %s\ngiven to reorder_axes",
                .daf_type_name(d), S7::prop(d, "name")
            ), call. = FALSE)
        }
    }

    shared_axes <- .compute_shared_planned_axes(dafs, permutations)

    sort_keys <- vapply(dafs, function(d) {
        p <- tryCatch(complete_path(d), error = function(e) NULL)
        if (!is.null(p)) sprintf("0\t%s", p)
        else sprintf("1\t%s", S7::prop(d, "name"))
    }, character(1L))
    sorted <- dafs[order(sort_keys)]

    for (d in sorted) {
        local_perms <- permutations[
            vapply(names(permutations), function(a) format_has_axis(d, a),
                   logical(1L))
        ]
        if (length(local_perms) == 0L) next
        plan <- .build_reorder_plan(d, local_perms, shared_axes = shared_axes)
        format_replace_reorder(d, plan, crash_counter = crash_counter)
        format_cleanup_reorder(d, plan, crash_counter = crash_counter)
    }

    invisible(dafs)
}

# Resolve `permutations` against every writer that has each axis,
# erroring if entry order differs (Julia's `axis: <a> entries differ`).
# Returns a named list `axis -> .reorder_planned_axis(perm, entries)`.
.compute_shared_planned_axes <- function(dafs, permutations) {
    out <- list()
    for (axis in names(permutations)) {
        ref_entries <- NULL; ref_name <- NULL
        for (d in dafs) {
            if (!format_has_axis(d, axis)) next
            entries <- format_axis_array(d, axis)$value
            if (is.null(ref_entries)) {
                ref_entries <- entries
                ref_name <- S7::prop(d, "name")
            } else if (!identical(unname(entries), unname(ref_entries))) {
                stop(sprintf(
                    "axis: %s entries differ\nbetween the daf data: %s\nand the daf data: %s",
                    axis, ref_name, S7::prop(d, "name")
                ), call. = FALSE)
            }
        }
        if (is.null(ref_entries)) {
            stop(sprintf(
                "axis: %s\ndoes not exist in any of the writers",
                axis
            ), call. = FALSE)
        }
        out[[axis]] <- .reorder_planned_axis(permutations[[axis]], ref_entries)
    }
    out
}

#' Reset any in-progress axis reorder.
#'
#' Idempotent. On `files_daf`, completes the rollback of any pending
#' reorder by restoring the backup hardlinks and removing the
#' `.reorder.backup/` directory. On `memory_daf`, no-op.
#'
#' Note: `files_daf(path, mode = "r+")` and `mode = "w+"` automatically
#' invoke this on open, so manual calls are usually unnecessary.
#'
#' @param daf A [DafWriter].
#' @param crash_counter Internal — for testing only.
#' @return Invisibly the input `daf`.
#' @examples
#' d <- memory_daf()
#' reset_reorder_axes(d)  # no-op for memory_daf
#' @export
reset_reorder_axes <- function(daf, crash_counter = NULL) {
    rolled_back <- format_reset_reorder(daf, crash_counter = crash_counter)
    invisible(isTRUE(rolled_back))
}
