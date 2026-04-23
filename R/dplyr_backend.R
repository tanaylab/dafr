#' @include classes.R queries.R readers.R writers.R dataframes.R
NULL

# ---- Internal constructor --------------------------------------------------

# State carried by a daf_axis_tbl:
#   daf         : DafReader — the backing daf
#   axis        : character — axis name
#   row_mask    : integer or NULL — positions into the full axis; NULL = full
#   col_select  : character or NULL — explicit column selection; NULL = default
#                 (show all stored vectors + all overrides)
#   overrides   : named list of vectors, each aligned to row_mask order
#   groups      : character — names of grouping variables
#
# Invariant: overrides[[nm]] is always aligned to the current row_mask.
# arrange() commits its permutation to row_mask (and reorders overrides).

new_daf_axis_tbl <- function(daf, axis,
                             row_mask = NULL,
                             col_select = NULL,
                             overrides = list(),
                             groups = character()) {
    if (!is_daf(daf)) stop("`daf` must be a DafReader", call. = FALSE)
    if (!has_axis(daf, axis)) {
        stop(sprintf("axis %s is not in the daf", sQuote(axis)), call. = FALSE)
    }
    structure(
        list(),
        daf = daf,
        axis = axis,
        row_mask = row_mask,
        col_select = col_select,
        overrides = overrides,
        groups = groups,
        class = "daf_axis_tbl"
    )
}

# ---- tbl() entry point -----------------------------------------------------

# S3 method bound to dplyr::tbl for class "dafr::DafReader" (the S7
# class string). Named without dots so roxygen doesn't confuse it for
# an S3 method declaration; the actual binding is done via
# registerS3method() in .onLoad.
#' @noRd
tbl_DafReader_axis <- function(src, axis, ...) {
    new_daf_axis_tbl(src, axis)
}

# ---- Realization -----------------------------------------------------------

# Apply row_mask, col_select, overrides, groups to produce a tibble.
# All vectors (stored and override) are emitted in row_mask order.
.realize <- function(x) {
    daf <- attr(x, "daf")
    axis <- attr(x, "axis")
    entries <- axis_entries(daf, axis)
    n <- length(entries)

    rm <- attr(x, "row_mask")
    if (is.null(rm)) rm <- seq_len(n)

    stored_cols <- vectors_set(daf, axis)
    sel <- attr(x, "col_select")
    sel_explicit <- !is.null(sel)
    if (!sel_explicit) sel <- stored_cols

    overrides <- attr(x, "overrides")

    cols <- list()
    cols[["name"]] <- entries[rm]
    for (nm in sel) {
        if (nm %in% names(overrides)) {
            cols[[nm]] <- unname(overrides[[nm]])
        } else if (nm %in% stored_cols) {
            v <- get_vector(daf, axis, nm)
            cols[[nm]] <- unname(v[rm])
        }
    }
    if (!sel_explicit) {
        extra <- setdiff(names(overrides), c(sel, "name"))
        for (nm in extra) cols[[nm]] <- unname(overrides[[nm]])
    }

    out <- tibble::as_tibble(cols)

    grps <- attr(x, "groups")
    if (length(grps)) out <- dplyr::group_by(out, !!!rlang::syms(grps))

    out
}

# ---- collect / as_tibble / print -------------------------------------------
#
# These S3 method bodies are named without dots so roxygen2 doesn't
# try to document them as methods. They are bound to their generics
# via registerS3method() in .onLoad (see R/zzz.R).

#' @noRd
collect_daf_axis_tbl <- function(x, ...) .realize(x)

#' @noRd
as_tibble_daf_axis_tbl <- function(x, ...) {
    out <- .realize(x)
    if (inherits(out, "grouped_df")) out <- dplyr::ungroup(out)
    out
}

#' @noRd
print_daf_axis_tbl <- function(x, n = 6L, ...) {
    daf <- attr(x, "daf")
    axis <- attr(x, "axis")
    rm <- attr(x, "row_mask")
    nrows <- if (is.null(rm)) axis_length(daf, axis) else length(rm)
    cat(sprintf("<daf_axis_tbl>  axis: %s  [%d rows]\n", axis, nrows))
    grps <- attr(x, "groups")
    if (length(grps)) cat("groups:", paste(grps, collapse = ", "), "\n")
    print(utils::head(.realize(x), n))
    invisible(x)
}

# ---- select / pull ---------------------------------------------------------

#' @noRd
select_daf_axis_tbl <- function(.data, ...) {
    daf <- attr(.data, "daf")
    axis <- attr(.data, "axis")
    available <- c(vectors_set(daf, axis), names(attr(.data, "overrides")))
    chosen <- tidyselect::eval_select(
        rlang::expr(c(...)),
        stats::setNames(seq_along(available), available)
    )
    attr(.data, "col_select") <- names(chosen)
    .data
}

#' @noRd
pull_daf_axis_tbl <- function(.data, var = -1, name = NULL, ...) {
    df <- .realize(.data)
    if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
    dplyr::pull(df, {{ var }}, name = {{ name }})
}

# ---- filter ----------------------------------------------------------------

#' @noRd
filter_daf_axis_tbl <- function(.data, ..., .by = NULL) {
    df <- .realize(.data)
    if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
    keep <- dplyr::filter(df, ...)
    daf <- attr(.data, "daf")
    axis <- attr(.data, "axis")
    # New row_mask: positions in the full axis of the kept entries.
    attr(.data, "row_mask") <- match(keep$name, axis_entries(daf, axis))
    # Reindex overrides (aligned to previous row_mask order) to match
    # the new row_mask order.
    overrides <- attr(.data, "overrides")
    if (length(overrides)) {
        local_idx <- match(keep$name, df$name)
        attr(.data, "overrides") <- lapply(overrides, function(v) v[local_idx])
    }
    .data
}

# ---- mutate ----------------------------------------------------------------

#' @noRd
mutate_daf_axis_tbl <- function(.data, ...) {
    df <- .realize(.data)
    if (inherits(df, "grouped_df")) {
        df2 <- dplyr::ungroup(dplyr::mutate(df, ...))
        df <- dplyr::ungroup(df)
    } else {
        df2 <- dplyr::mutate(df, ...)
    }
    old_names <- colnames(df)
    new_or_changed <- setdiff(colnames(df2), "name")
    overrides <- attr(.data, "overrides")
    for (nm in new_or_changed) {
        if (!(nm %in% old_names) || !identical(df[[nm]], df2[[nm]])) {
            overrides[[nm]] <- df2[[nm]]
        }
    }
    attr(.data, "overrides") <- overrides
    .data
}

# ---- arrange / distinct ----------------------------------------------------

#' @noRd
arrange_daf_axis_tbl <- function(.data, ..., .by_group = FALSE) {
    df <- .realize(.data)
    if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
    sorted <- dplyr::arrange(df, ...)
    local_idx <- match(sorted$name, df$name)
    daf <- attr(.data, "daf")
    axis <- attr(.data, "axis")
    old_rm <- attr(.data, "row_mask")
    if (is.null(old_rm)) old_rm <- seq_len(axis_length(daf, axis))
    attr(.data, "row_mask") <- old_rm[local_idx]
    overrides <- attr(.data, "overrides")
    if (length(overrides)) {
        attr(.data, "overrides") <- lapply(overrides, function(v) v[local_idx])
    }
    .data
}

#' @noRd
distinct_daf_axis_tbl <- function(.data, ..., .keep_all = FALSE) {
    df <- .realize(.data)
    if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
    dplyr::distinct(df, ..., .keep_all = .keep_all)
}

# ---- group_by / summarise --------------------------------------------------

#' @noRd
group_by_daf_axis_tbl <- function(.data, ..., .add = FALSE,
                                  .drop = dplyr::group_by_drop_default(.data)) {
    quos <- rlang::enquos(...)
    vars <- unname(vapply(quos, rlang::as_name, character(1)))
    prev <- attr(.data, "groups")
    attr(.data, "groups") <- if (isTRUE(.add)) unique(c(prev, vars)) else vars
    .data
}

#' @noRd
ungroup_daf_axis_tbl <- function(x, ...) {
    attr(x, "groups") <- character()
    x
}

# ---- compute() — explicit write-back --------------------------------------

#' Persist `mutate()`-produced columns as daf vectors.
#'
#' `compute()` is the explicit write-back step for a `daf_axis_tbl`.
#' It accepts only columns that currently exist in the tbl's
#' overrides (i.e. introduced by [dplyr::mutate()]) and writes each as
#' a vector on the tbl's axis via [set_vector()].
#'
#' Write-back requires **full row coverage** of the axis. If the tbl
#' has been [dplyr::filter()]ed (partial row mask), `compute()` errors
#' rather than silently persist partial data. A permuted-but-full row
#' mask (e.g. after [dplyr::arrange()]) is un-permuted to axis order
#' before writing.
#'
#' @param x A `daf_axis_tbl`.
#' @param vectors Character vector of override column names to persist.
#'   Required — passing `NULL` errors, by design, to avoid silent
#'   whole-tbl writes.
#' @param overwrite Passed to [set_vector()]. Defaults to `FALSE`.
#' @return Invisibly, `x`.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "cell", c("c1", "c2", "c3"))
#' set_vector(d, "cell", "n", c(10, 20, 30))
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'     t <- dplyr::tbl(d, "cell") |> dplyr::mutate(log_n = log10(n))
#'     compute(t, vectors = "log_n")
#'     has_vector(d, "cell", "log_n")  # TRUE
#' }
#' @export
compute <- function(x, vectors = NULL, overwrite = FALSE) {
    UseMethod("compute")
}

#' @export
compute.daf_axis_tbl <- function(x, vectors = NULL, overwrite = FALSE) {
    if (is.null(vectors) || !length(vectors)) {
        stop("compute(): `vectors` must name override columns to persist",
             call. = FALSE)
    }
    overrides <- attr(x, "overrides")
    missing <- setdiff(vectors, names(overrides))
    if (length(missing)) {
        stop(sprintf(
            "compute(): not present as an override: %s. Create with mutate().",
            paste(sQuote(missing), collapse = ", ")
        ), call. = FALSE)
    }
    daf <- attr(x, "daf")
    axis <- attr(x, "axis")
    rm <- attr(x, "row_mask")
    n <- axis_length(daf, axis)
    if (is.null(rm)) {
        reorder_idx <- NULL
    } else if (length(rm) == n && !anyDuplicated(rm)) {
        reorder_idx <- order(rm)
    } else {
        stop("compute(): tbl has a partial row mask (filtered/reduced); cannot write back a full vector. Undo the filter or explicitly extend.",
             call. = FALSE)
    }
    for (nm in vectors) {
        v <- overrides[[nm]]
        if (!is.null(reorder_idx)) v <- v[reorder_idx]
        set_vector(daf, axis, nm, v, overwrite = overwrite)
    }
    invisible(x)
}

#' @noRd
summarise_daf_axis_tbl <- function(.data, ..., .by = NULL, .groups = NULL) {
    df <- .realize(.data)
    grps <- attr(.data, "groups")
    if (length(grps) == 0) {
        if (inherits(df, "grouped_df")) df <- dplyr::ungroup(df)
        return(dplyr::summarise(df, ...))
    }
    result <- dplyr::summarise(df, ..., .groups = "drop")
    daf <- attr(.data, "daf")
    # Auto-tie-back: single group var, that var names an axis in the daf.
    if (length(grps) == 1L && has_axis(daf, grps)) {
        axis_name <- grps
        all_entries <- axis_entries(daf, axis_name)
        group_col <- result[[axis_name]]
        if (is.character(group_col) && all(group_col %in% all_entries)) {
            rm <- match(group_col, all_entries)
            derived <- as.list(result[, setdiff(colnames(result), axis_name),
                drop = FALSE])
            return(new_daf_axis_tbl(
                daf = daf,
                axis = axis_name,
                row_mask = rm,
                col_select = names(derived),
                overrides = derived
            ))
        }
    }
    result
}
