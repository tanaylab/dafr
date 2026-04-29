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
#'   [tick_crash_counter()] (not exported).
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
    plan <- .build_reorder_plan(daf, permutations)
    format_replace_reorder(daf, plan, crash_counter = crash_counter)
    format_cleanup_reorder(daf, plan, crash_counter = crash_counter)
    invisible(daf)
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
    format_reset_reorder(daf, crash_counter = crash_counter)
    invisible(daf)
}
