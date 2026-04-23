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
