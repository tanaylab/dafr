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

# S3 method registered against dplyr::tbl for class "dafr::DafReader"
# (the S7 name). Registration happens in .onLoad.
tbl.dafr__DafReader <- function(src, axis, ...) {
    new_daf_axis_tbl(src, axis)
}
